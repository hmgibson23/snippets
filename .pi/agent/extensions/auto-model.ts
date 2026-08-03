/**
 * Auto Model Extension
 *
 * Dynamically routes prompts to cheap or smart models based on complexity.
 * Inspired by VS Code Copilot's "auto" model selection.
 *
 * Usage:
 *   /auto          — toggle on/off
 *   /auto on       — enable
 *   /auto off      — disable
 *   /auto status   — show current state
 *   /auto cheap provider/model  — set cheap model
 *   /auto smart provider/model  — set smart model
 *
 * Config: ~/.pi/agent/auto-model.json (optional)
 * ```json
 * {
 *   "cheap": "github-copilot/claude-haiku-4.5",
 *   "smart": "github-copilot/claude-sonnet-4.6",
 *   "simpleWordThreshold": 20,
 *   "complexCharThreshold": 200,
 *   "complexKeywords": ["refactor", "review", "analyze"],
 *   "simpleKeywords": ["what is", "show me", "list"]
 * }
 * ```
 */

import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { getAgentDir } from "@earendil-works/pi-coding-agent";

// ── Defaults ────────────────────────────────────────────────────────────────

const DEFAULT_CHEAP = "github-copilot/claude-haiku-4.5";
const DEFAULT_SMART = "github-copilot/claude-sonnet-4.6";

const DEFAULT_COMPLEX_KEYWORDS = [
  "refactor", "review", "analyze", "implement", "build", "create",
  "design", "architecture", "complex", "debug", "optimize", "restructure",
  "migrate", "redesign", "test", "configure", "setup", "deploy",
  "write code", "edit", "change", "add feature", "fix bug", "investigate",
  "redesign",
];

const DEFAULT_SIMPLE_KEYWORDS = [
  "what is", "what's", "show me", "list", "find", "grep", "where is",
  "how do i", "explain", "summarize", "tell me", "help",
  "ls", "cat", "head", "tail",
];

// ── Config ──────────────────────────────────────────────────────────────────

interface AutoModelConfig {
  cheap?: string;
  smart?: string;
  simpleWordThreshold?: number;
  complexCharThreshold?: number;
  complexKeywords?: string[];
  simpleKeywords?: string[];
}

function loadConfig(): AutoModelConfig {
  const path = join(getAgentDir(), "auto-model.json");
  if (!existsSync(path)) return {};
  try {
    return JSON.parse(readFileSync(path, "utf-8"));
  } catch {
    return {};
  }
}

function parseModelRef(ref: string): { provider: string; id: string } | undefined {
  const idx = ref.indexOf("/");
  if (idx < 0) return undefined;
  return { provider: ref.slice(0, idx), id: ref.slice(idx + 1) };
}

// ── Extension ───────────────────────────────────────────────────────────────

export default function autoModelExtension(pi: ExtensionAPI) {
  const config = loadConfig();

  let enabled = false;
  let cheap = config.cheap ?? DEFAULT_CHEAP;
  let smart = config.smart ?? DEFAULT_SMART;
  let simpleWordThreshold = config.simpleWordThreshold ?? 20;
  let complexCharThreshold = config.complexCharThreshold ?? 200;
  let complexKeywords = config.complexKeywords ?? DEFAULT_COMPLEX_KEYWORDS;
  let simpleKeywords = config.simpleKeywords ?? DEFAULT_SIMPLE_KEYWORDS;
  let lastDecision: "cheap" | "smart" | undefined;

  // ── Complexity Analysis ────────────────────────────────────────────────

  function isComplex(text: string): boolean {
    const lower = text.toLowerCase().trim();
    const words = lower.split(/\s+/).filter(Boolean);
    const wordCount = words.length;

    // If it matches a simple keyword AND is short → cheap
    if (
      wordCount < simpleWordThreshold &&
      simpleKeywords.some((kw) => lower.includes(kw))
    ) {
      return false;
    }

    // Long prompts → smart
    if (text.length > complexCharThreshold) return true;

    // Complex keyword match → smart
    if (complexKeywords.some((kw) => lower.includes(kw))) return true;

    // Default: short = simple, long = complex
    return wordCount >= simpleWordThreshold;
  }

  // ── Model Switching ────────────────────────────────────────────────────

  async function switchModel(
    ref: string,
    _label: string,
    ctx: ExtensionContext,
  ): Promise<void> {
    const parsed = parseModelRef(ref);
    if (!parsed) return;

    const current = ctx.model;
    if (current && current.provider === parsed.provider && current.id === parsed.id) return;

    const model = ctx.modelRegistry.find(parsed.provider, parsed.id);
    if (model) {
      await pi.setModel(model);
    }
  }

  // ── Status Indicator ───────────────────────────────────────────────────

  function updateStatus(ctx: ExtensionContext) {
    const theme = ctx.ui.theme;
    if (enabled) {
      // cheap = info (blue/green), smart = warning (yellow)
      const colour = lastDecision === "cheap" ? "info" : "warning";
      ctx.ui.setStatus("auto-model", theme.fg(colour, "auto"));
    } else {
      ctx.ui.setStatus("auto-model", undefined);
    }
  }

  // ── /auto Command ──────────────────────────────────────────────────────

  pi.registerCommand("auto", {
    description:
      "Toggle auto model selection. Usage: /auto [on|off|status|cheap <model>|smart <model>]",
    handler: async (args, ctx) => {
      const arg = args?.trim() || "";

      // /auto status
      if (arg === "status") {
        ctx.ui.notify(
          [
            `auto-model: ${enabled ? "🟢 ON" : "⚫ OFF"}`,
            `  cheap  → ${cheap}`,
            `  smart  → ${smart}`,
            `  simple ≤ ${simpleWordThreshold} words  |  complex ≥ ${complexCharThreshold} chars`,
          ].join("\n"),
          "info",
        );
        return;
      }

      // /auto on / /auto enable
      if (arg === "on" || arg === "enable") {
        enabled = true;
        ctx.ui.notify(`🔀 auto-model ON — cheap: ${cheap}  smart: ${smart}`, "info");
        updateStatus(ctx);
        return;
      }

      // /auto off / /auto disable
      if (arg === "off" || arg === "disable") {
        enabled = false;
        lastDecision = undefined;
        ctx.ui.notify("auto-model OFF", "info");
        updateStatus(ctx);
        return;
      }

      // /auto cheap <provider/model>
      if (arg.startsWith("cheap ")) {
        const ref = arg.slice(6).trim();
        if (parseModelRef(ref)) {
          cheap = ref;
          ctx.ui.notify(`auto cheap → ${cheap}`, "info");
        } else {
          ctx.ui.notify(`Invalid model ref "${ref}". Use format: provider/model-id`, "error");
        }
        return;
      }

      // /auto smart <provider/model>
      if (arg.startsWith("smart ")) {
        const ref = arg.slice(6).trim();
        if (parseModelRef(ref)) {
          smart = ref;
          ctx.ui.notify(`auto smart → ${smart}`, "info");
        } else {
          ctx.ui.notify(`Invalid model ref "${ref}". Use format: provider/model-id`, "error");
        }
        return;
      }

      // Unknown subcommand
      if (arg && !["on", "off", "enable", "disable", "status", "cheap", "smart", ""].includes(arg)) {
        ctx.ui.notify(
          `Unknown "${arg}". Usage: /auto [on|off|status|cheap <model>|smart <model>]`,
          "error",
        );
        return;
      }

      // /auto (bare) → toggle
      enabled = !enabled;
      if (!enabled) lastDecision = undefined;
      ctx.ui.notify(`auto-model ${enabled ? "ON" : "OFF"}`, "info");
      updateStatus(ctx);
    },
  });

  // ── Route Before Each Agent Turn ───────────────────────────────────────

  pi.on("before_agent_start", async (event, ctx) => {
    if (!enabled) return;

    const text = event.prompt || "";
    if (!text.trim()) return;

    const complex = isComplex(text);
    const target = complex ? smart : cheap;
    lastDecision = complex ? "smart" : "cheap";

    await switchModel(target, lastDecision, ctx);
    updateStatus(ctx);
  });

  // ── Init Status on Session Start ───────────────────────────────────────

  pi.on("session_start", async (_event, ctx) => {
    updateStatus(ctx);
  });
}
