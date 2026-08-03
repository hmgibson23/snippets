import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { buildPiTitle, titleFromPrompt } from "./title";

export default function smartTitleExtension(pi: ExtensionAPI) {
  let manualTitle: string | undefined;
  let lastTask: string | undefined;

  function currentTask(): string | undefined {
    return manualTitle || lastTask || pi.getSessionName() || undefined;
  }

  function updateTitle(ctx: ExtensionContext) {
    if (!ctx.hasUI) return;
    ctx.ui.setTitle(buildPiTitle({ task: currentTask(), cwd: ctx.cwd }));
  }

  pi.on("session_start", async (_event, ctx) => {
    updateTitle(ctx);
  });

  pi.on("before_agent_start", async (event, ctx) => {
    if (!manualTitle) {
      lastTask = titleFromPrompt(event.prompt);
    }
    updateTitle(ctx);
  });

  pi.registerCommand("smart-title", {
    description: "Set, show, or reset the smart terminal title (usage: /smart-title [set <title>|reset|show])",
    handler: async (args, ctx) => {
      const trimmed = args.trim();
      const [action, ...rest] = trimmed.split(/\s+/);

      if (!trimmed || action === "show") {
        updateTitle(ctx);
        ctx.ui.notify(`Title: ${buildPiTitle({ task: currentTask(), cwd: ctx.cwd })}`, "info");
        return;
      }

      if (action === "reset") {
        manualTitle = undefined;
        lastTask = undefined;
        updateTitle(ctx);
        ctx.ui.notify("Smart title reset", "info");
        return;
      }

      if (action === "set") {
        const title = rest.join(" ").trim();
        if (!title) {
          ctx.ui.notify("Usage: /smart-title set <title>", "error");
          return;
        }
        manualTitle = titleFromPrompt(title);
        updateTitle(ctx);
        ctx.ui.notify(`Smart title set: ${manualTitle}`, "info");
        return;
      }

      manualTitle = titleFromPrompt(trimmed);
      updateTitle(ctx);
      ctx.ui.notify(`Smart title set: ${manualTitle}`, "info");
    },
  });
}
