import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, relative } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";
import { appendDiscoveryEntry, buildDecisionMemory, findProjectNapkinVault, slugify } from "./helpers";

const TOOL_NAME = "napkin_capture";
const STATUS_KEY = "napkin-auto-memory";
const DISCOVERIES_PATH = "guides/discoveries.md";

const CaptureParams = Type.Object({
  type: Type.Union([Type.Literal("decision"), Type.Literal("discovery")], {
    description: "Memory type to persist",
  }),
  title: Type.String({ description: "Short title for the decision or discovery" }),
  summary: Type.String({ description: "1-3 sentence summary of the durable memory" }),
  rationale: Type.Optional(
    Type.String({ description: "Why this matters or why the choice was made. Required for decisions." }),
  ),
  evidence: Type.String({ description: "Concrete evidence from tool output, code, docs, or explicit user confirmation" }),
  files: Type.Optional(Type.Array(Type.String({ description: "Relevant repo-relative file paths" }))),
  confidence: Type.Union([Type.Literal("high")], {
    description: "Only high-confidence memories should be captured",
  }),
});

function today() {
  return new Date().toISOString().slice(0, 10);
}

function normalizeFiles(files: string[] | undefined, vaultPath: string): string[] {
  return (files ?? [])
    .map((file) => file.trim())
    .filter(Boolean)
    .map((file) => (file.startsWith(vaultPath) ? relative(vaultPath, file) : file))
    .filter((file, index, arr) => arr.indexOf(file) === index);
}

function setNapkinStatus(ctx: ExtensionContext) {
  const vaultPath = findProjectNapkinVault(process.cwd());
  if (vaultPath) {
    ctx.ui.setStatus(STATUS_KEY, `Napkin: ${relative(process.cwd(), vaultPath) || "."}`);
  }
}

function ensureUniqueDecisionPath(vaultPath: string, preferredRelativePath: string, content: string) {
  const preferredAbsolutePath = join(vaultPath, preferredRelativePath);
  if (!existsSync(preferredAbsolutePath)) {
    return { absolutePath: preferredAbsolutePath, relativePath: preferredRelativePath, skipped: false };
  }

  const existingContent = readFileSync(preferredAbsolutePath, "utf8");
  if (existingContent === content) {
    return { absolutePath: preferredAbsolutePath, relativePath: preferredRelativePath, skipped: true };
  }

  const base = preferredRelativePath.replace(/\.md$/, "");
  for (let index = 2; index < 100; index += 1) {
    const candidateRelativePath = `${base}-${index}.md`;
    const candidateAbsolutePath = join(vaultPath, candidateRelativePath);
    if (!existsSync(candidateAbsolutePath)) {
      return { absolutePath: candidateAbsolutePath, relativePath: candidateRelativePath, skipped: false };
    }
  }

  throw new Error(`Could not find a unique path for ${preferredRelativePath}`);
}

function writeDecision(vaultPath: string, params: {
  title: string;
  summary: string;
  rationale?: string;
  evidence: string;
  files?: string[];
}) {
  if (!params.rationale?.trim()) {
    throw new Error("Decision captures require rationale");
  }

  const memory = buildDecisionMemory({
    date: today(),
    title: params.title,
    summary: params.summary,
    rationale: params.rationale,
    evidence: params.evidence,
    files: normalizeFiles(params.files, vaultPath),
  });

  const resolved = ensureUniqueDecisionPath(vaultPath, memory.relativePath, memory.content);
  if (resolved.skipped) {
    return { action: "skipped" as const, relativePath: resolved.relativePath };
  }

  mkdirSync(dirname(resolved.absolutePath), { recursive: true });
  writeFileSync(resolved.absolutePath, memory.content, "utf8");
  return { action: "created" as const, relativePath: resolved.relativePath };
}

function writeDiscovery(vaultPath: string, params: {
  title: string;
  summary: string;
  evidence: string;
  files?: string[];
}) {
  const discoveryAbsolutePath = join(vaultPath, DISCOVERIES_PATH);
  const existingContent = existsSync(discoveryAbsolutePath) ? readFileSync(discoveryAbsolutePath, "utf8") : "";
  const next = appendDiscoveryEntry(existingContent, {
    date: today(),
    title: params.title,
    summary: params.summary,
    evidence: params.evidence,
    files: normalizeFiles(params.files, vaultPath),
  });

  if (!next.didAppend) {
    return { action: "skipped" as const, relativePath: DISCOVERIES_PATH };
  }

  mkdirSync(dirname(discoveryAbsolutePath), { recursive: true });
  writeFileSync(discoveryAbsolutePath, next.nextContent, "utf8");
  return { action: "updated" as const, relativePath: DISCOVERIES_PATH };
}

export default function napkinAutoMemory(pi: ExtensionAPI) {
  pi.on("session_start", async (_event, ctx) => {
    setNapkinStatus(ctx);
  });

  pi.on("before_agent_start", async (event, _ctx) => {
    const vaultPath = findProjectNapkinVault(process.cwd());
    if (!vaultPath) {
      return;
    }

    return {
      systemPrompt:
        `${event.systemPrompt}\n\n` +
        [
          "Napkin auto-memory is available for this project.",
          `Use the ${TOOL_NAME} tool automatically when — and only when — you have a high-confidence, milestone-level memory worth saving to the project's local Napkin vault.`,
          "Capture only:",
          "- decisions that were clearly made with rationale",
          "- discoveries that were verified by tool output, code inspection, docs, or explicit user confirmation",
          "Do not capture uncertain ideas, tentative suggestions, or generic progress updates.",
          "For decisions, include rationale. For discoveries, include the concrete evidence. Prefer repo-relative file paths when relevant.",
        ].join("\n"),
    };
  });

  pi.registerTool({
    name: TOOL_NAME,
    label: "Napkin Capture",
    description:
      "Persist a high-confidence project decision or verified discovery into the local Napkin vault. Use automatically at milestone boundaries; skip uncertain cases.",
    parameters: CaptureParams,
    async execute(_toolCallId, params, _signal, _onUpdate, _ctx) {
      const vaultPath = findProjectNapkinVault(process.cwd());
      if (!vaultPath) {
        return {
          content: [{ type: "text", text: "Skipped: no project-local .napkin vault found." }],
          details: { skipped: true, reason: "no-project-vault" },
        };
      }

      const safeTitle = params.title.trim() || slugify(params.summary).slice(0, 60) || "untitled-memory";

      const result =
        params.type === "decision"
          ? writeDecision(vaultPath, {
              title: safeTitle,
              summary: params.summary,
              rationale: params.rationale,
              evidence: params.evidence,
              files: params.files,
            })
          : writeDiscovery(vaultPath, {
              title: safeTitle,
              summary: params.summary,
              evidence: params.evidence,
              files: params.files,
            });

      return {
        content: [
          {
            type: "text",
            text:
              result.action === "skipped"
                ? `Napkin capture skipped duplicate ${params.type}: ${result.relativePath}`
                : `Napkin capture ${result.action} ${params.type}: ${result.relativePath}`,
          },
        ],
        details: {
          vaultPath,
          type: params.type,
          action: result.action,
          relativePath: result.relativePath,
        },
      };
    },
  });

  pi.registerCommand("napkin-status", {
    description: "Show the current project-local Napkin vault used by napkin auto-memory",
    handler: async (_args, ctx) => {
      const vaultPath = findProjectNapkinVault(process.cwd());
      if (!vaultPath) {
        ctx.ui.notify("No project-local .napkin vault found from the current working directory", "warning");
        return;
      }

      ctx.ui.notify(`Napkin auto-memory writing to ${vaultPath}`, "info");
    },
  });
}
