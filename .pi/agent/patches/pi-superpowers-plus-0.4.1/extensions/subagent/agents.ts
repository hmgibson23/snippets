/**
 * Agent discovery and configuration
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { parseFrontmatter } from "@mariozechner/pi-coding-agent";
import { log } from "../logging.js";

export type AgentScope = "user" | "project" | "both";

export const SUBAGENT_THINKING_LEVELS = ["off", "minimal", "low", "medium", "high", "xhigh", "max"] as const;
export type SubagentThinkingLevel = (typeof SUBAGENT_THINKING_LEVELS)[number];

export interface AgentInvocationOverrides {
  /** Exact `provider/model` to use for this invocation instead of the profile default. */
  model?: string;
  /** Thinking level to use for this invocation instead of the profile default. */
  thinking?: SubagentThinkingLevel;
}

export interface AgentConfig {
  name: string;
  description: string;
  tools?: string[];
  extensions?: string[];
  model?: string;
  /** Pi CLI thinking level, parsed from frontmatter `thinking`. */
  thinking?: SubagentThinkingLevel;
  /** Absolute run-time ceiling in milliseconds, parsed from frontmatter `timeout` (minutes). */
  timeoutMs?: number;
  systemPrompt: string;
  source: "user" | "project";
  filePath: string;
}

export interface AgentDiscoveryResult {
  agents: AgentConfig[];
  projectAgentsDir: string | null;
}

/**
 * Providers bundled in pi-ai's core provider catalog (`@earendil-works/pi-ai/dist/providers/data/*.json`
 * as of pi-coding-agent 0.84.x) — these work with zero user extensions loaded, verified empirically
 * (`pi --no-extensions --model <provider>/<model> ...` for deepseek, openai-codex, github-copilot all
 * produced a normal completion with tool calls working). Providers NOT in this list (e.g. `mimo`, which
 * is registered by the `@lesetong/pi-mimo` extension package) fail under `--no-extensions` — confirmed
 * empirically too (mimo fell through to an unrelated huggingface auth error with no models resolved).
 * Re-verify by listing that directory or testing directly if pi-coding-agent updates and this drifts.
 */
const CORE_PROVIDERS = new Set([
  "openai",
  "openai-codex",
  "openrouter",
  "together",
  "baseten",
  "deepseek",
  "zai",
  "zai-coding-cn",
  "qwen-token-plan",
  "qwen-token-plan-cn",
  "qwen-token-plan-individual",
  "ant-ling",
  "anthropic",
  "github-copilot",
  "google",
  "google-vertex",
  "azure-openai-responses",
  "amazon-bedrock",
  "cloudflare-ai-gateway",
  "cloudflare-workers-ai",
  "cerebras",
  "fireworks",
  "groq",
  "huggingface",
  "kimi-coding",
  "minimax",
  "minimax-cn",
  "mistral",
  "moonshotai",
  "moonshotai-cn",
  "nvidia",
  "opencode",
  "opencode-go",
  "vercel-ai-gateway",
  "xai",
  "xiaomi",
  "xiaomi-token-plan-ams",
  "xiaomi-token-plan-cn",
  "xiaomi-token-plan-sgp",
]);

/** Built-in tools that exist with zero user extensions loaded. Anything outside this set
 * (e.g. `ffgrep`/`fffind` from pi-fff, `map`/`search`/`outline` from pi-codemapper, `subagent`
 * itself, any MCP tool) requires extension discovery to be enabled. */
const CORE_TOOLS = new Set(["read", "write", "edit", "bash", "grep", "find", "ls"]);

/**
 * Whether this agent can safely run with `--no-extensions` (skips loading every configured
 * package/MCP server — measured ~4.5s+ of pure startup overhead per subagent spawn on a trivial
 * task, more once real extensions like repo-indexing tools are in play, plus a class of "finished
 * but the process won't exit" hangs from extensions with lingering timers/connections). Requires
 * BOTH an explicitly pinned model on a core provider AND an explicit tools list that's a subset of
 * the core built-ins — an agent with no explicit `tools:` (meaning "give me everything") or any
 * non-core tool/provider keeps the current full-extensions behavior.
 */
export function canFastStart(agent: AgentConfig): boolean {
  if (!agent.model) return false;
  const provider = agent.model.split("/")[0];
  if (!CORE_PROVIDERS.has(provider)) return false;
  if (!agent.tools || agent.tools.length === 0) return false;
  return agent.tools.every((t) => CORE_TOOLS.has(t));
}

/** Return a per-run effective profile without mutating the discovered profile. */
export function withInvocationOverrides(agent: AgentConfig, overrides?: AgentInvocationOverrides): AgentConfig {
  if (!overrides?.model && !overrides?.thinking) return agent;
  return {
    ...agent,
    model: overrides.model ?? agent.model,
    thinking: overrides.thinking ?? agent.thinking,
  };
}

export function loadAgentsFromDir(dir: string, source: "user" | "project"): AgentConfig[] {
  const agents: AgentConfig[] = [];

  if (!fs.existsSync(dir)) {
    return agents;
  }

  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch (err) {
    log.warn(`Failed to read agents directory: ${dir} — ${err instanceof Error ? err.message : err}`);
    return agents;
  }

  for (const entry of entries) {
    if (!entry.name.endsWith(".md")) continue;
    if (!entry.isFile() && !entry.isSymbolicLink()) continue;

    const filePath = path.join(dir, entry.name);
    let content: string;
    try {
      content = fs.readFileSync(filePath, "utf-8");
    } catch (err) {
      log.warn(`Failed to read agent file: ${filePath} — ${err instanceof Error ? err.message : err}`);
      continue;
    }

    const { frontmatter, body } = parseFrontmatter<Record<string, string>>(content);

    if (!frontmatter.name || !frontmatter.description) {
      continue;
    }

    const tools = frontmatter.tools
      ?.split(",")
      .map((t: string) => t.trim())
      .filter(Boolean);
    const extensions = frontmatter.extensions
      ?.split(",")
      .map((t: string) => t.trim())
      .filter(Boolean);

    let thinking: SubagentThinkingLevel | undefined;
    if (frontmatter.thinking) {
      if ((SUBAGENT_THINKING_LEVELS as readonly string[]).includes(frontmatter.thinking)) {
        thinking = frontmatter.thinking as SubagentThinkingLevel;
      } else {
        log.warn(
          `Ignoring invalid "thinking" frontmatter in ${filePath}: ${frontmatter.thinking}. ` +
            `Expected one of: ${SUBAGENT_THINKING_LEVELS.join(", ")}`,
        );
      }
    }

    let timeoutMs: number | undefined;
    if (frontmatter.timeout) {
      const minutes = Number.parseFloat(frontmatter.timeout);
      if (Number.isFinite(minutes) && minutes > 0) {
        timeoutMs = Math.round(minutes * 60_000);
      } else {
        log.warn(`Ignoring invalid "timeout" frontmatter (expected minutes as a number) in ${filePath}: ${frontmatter.timeout}`);
      }
    }

    agents.push({
      name: frontmatter.name,
      description: frontmatter.description,
      tools: tools && tools.length > 0 ? tools : undefined,
      extensions: extensions && extensions.length > 0 ? extensions : undefined,
      model: frontmatter.model,
      thinking,
      timeoutMs,
      systemPrompt: body,
      source,
      filePath,
    });
  }

  return agents;
}

function isDirectory(p: string): boolean {
  try {
    return fs.statSync(p).isDirectory();
  } catch (err) {
    log.debug(`stat failed for ${p}: ${err instanceof Error ? err.message : err}`);
    return false;
  }
}

function findNearestProjectAgentsDir(cwd: string): string | null {
  let currentDir = cwd;
  while (true) {
    const candidate = path.join(currentDir, ".pi", "agents");
    if (isDirectory(candidate)) return candidate;

    const parentDir = path.dirname(currentDir);
    if (parentDir === currentDir) return null;
    currentDir = parentDir;
  }
}

export function discoverAgents(cwd: string, scope: AgentScope): AgentDiscoveryResult {
  const userDir = path.join(os.homedir(), ".pi", "agent", "agents");
  const projectAgentsDir = findNearestProjectAgentsDir(cwd);
  const thisFile = fileURLToPath(import.meta.url);
  const packageRoot = path.resolve(path.dirname(thisFile), "..", "..");
  const bundledAgentsDir = path.join(packageRoot, "agents");

  const userAgents = scope === "project" ? [] : loadAgentsFromDir(userDir, "user");
  const projectAgents = scope === "user" || !projectAgentsDir ? [] : loadAgentsFromDir(projectAgentsDir, "project");
  const bundledAgents = scope === "user" ? [] : loadAgentsFromDir(bundledAgentsDir, "project");

  const agentMap = new Map<string, AgentConfig>();

  if (scope === "both") {
    for (const agent of bundledAgents) agentMap.set(agent.name, agent);
    for (const agent of userAgents) agentMap.set(agent.name, agent);
    for (const agent of projectAgents) agentMap.set(agent.name, agent);
  } else if (scope === "user") {
    for (const agent of userAgents) agentMap.set(agent.name, agent);
  } else {
    for (const agent of bundledAgents) agentMap.set(agent.name, agent);
    for (const agent of projectAgents) agentMap.set(agent.name, agent);
  }

  return { agents: Array.from(agentMap.values()), projectAgentsDir };
}

export function formatAgentList(agents: AgentConfig[], maxItems: number): { text: string; remaining: number } {
  if (agents.length === 0) return { text: "none", remaining: 0 };
  const listed = agents.slice(0, maxItems);
  const remaining = agents.length - listed.length;
  return {
    text: listed.map((a) => `${a.name} (${a.source}): ${a.description}`).join("; "),
    remaining,
  };
}
