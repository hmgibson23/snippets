import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { buildHandoffPrompt, summarizeBranch } from "./handoff";

export default function handoffPackExtension(pi: ExtensionAPI) {
  pi.registerCommand("handoff", {
    description: "Create a focused continuation prompt in a new session (usage: /handoff <next goal>)",
    handler: async (args, ctx) => {
      if (!ctx.hasUI) {
        ctx.ui.notify("handoff requires interactive mode", "error");
        return;
      }

      const branch = ctx.sessionManager.getBranch();
      const summary = summarizeBranch(branch);
      const prompt = buildHandoffPrompt({
        goal: args.trim(),
        cwd: ctx.cwd,
        sessionFile: ctx.sessionManager.getSessionFile() ?? undefined,
        summary,
      });

      const editedPrompt = await ctx.ui.editor("Edit handoff prompt", prompt);
      if (editedPrompt === undefined) {
        ctx.ui.notify("Handoff cancelled", "info");
        return;
      }

      const currentSessionFile = ctx.sessionManager.getSessionFile();
      const result = await ctx.newSession({
        parentSession: currentSessionFile,
        withSession: async (replacementCtx) => {
          replacementCtx.ui.setEditorText(editedPrompt);
          replacementCtx.ui.notify("Handoff ready. Submit when ready.", "info");
        },
      });

      if (result.cancelled) {
        ctx.ui.notify("New session cancelled", "info");
      }
    },
  });
}
