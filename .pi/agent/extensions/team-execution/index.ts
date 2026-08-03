import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";
import {
  buildExecutionInstructions,
  buildStatusView,
  createDefaultState,
  deriveStateFromTeamsTool,
  extractToolDetails,
  type TeamExecutionMode,
  type TeamExecutionState,
} from "./logic";

const ENTRY_TYPE = "team-execution-state";

function cloneState(state: TeamExecutionState): TeamExecutionState {
  return { ...state };
}

function isStateLike(value: unknown): value is Partial<TeamExecutionState> {
  return typeof value === "object" && value !== null;
}

function mergeState(current: TeamExecutionState, next: Partial<TeamExecutionState>): TeamExecutionState {
  return {
    ...current,
    ...next,
    continueUsingTeamByDefault: next.continueUsingTeamByDefault ?? current.continueUsingTeamByDefault,
    observabilityAttached: next.observabilityAttached ?? current.observabilityAttached,
  };
}

export default function teamExecution(pi: ExtensionAPI) {
  let state = createDefaultState();

  function persistState() {
    pi.appendEntry(ENTRY_TYPE, state);
  }

  function renderStatus(ctx: { hasUI: boolean; ui: { setStatus: (id: string, text?: string) => void } }) {
    if (!ctx.hasUI) return;
    const view = buildStatusView(state);
    ctx.ui.setStatus("team-execution", view.label || undefined);
  }

  pi.registerTool({
    name: "team_execution_status",
    label: "Team Execution Status",
    description: "Inspect the current team execution mode, whether Pi should keep reusing the team, and the last recorded fallback reason.",
    promptSnippet: "Inspect the current team execution context and fallback reason.",
    promptGuidelines: [
      "Use this tool when the user asks why Pi is or is not still using the team, or when you need the current team execution mode before coordinating work.",
    ],
    parameters: Type.Object({}),
    async execute() {
      const view = buildStatusView(state);
      const lines = [
        `mode: ${state.mode}`,
        `teamId: ${state.teamId ?? "(unknown)"}`,
        `activeSince: ${state.activeSince ?? "(unknown)"}`,
        `lastTeamAction: ${state.lastTeamAction ?? "(none)"}`,
        `lastTeamActionAt: ${state.lastTeamActionAt ?? "(unknown)"}`,
        `continueUsingTeamByDefault: ${state.continueUsingTeamByDefault ? "yes" : "no"}`,
        `observabilityAttached: ${state.observabilityAttached ? "yes" : "no"}`,
        `summary: ${view.summary}`,
      ];
      if (state.lastReason) lines.push(`reason: ${state.lastReason}`);
      return {
        content: [{ type: "text", text: lines.join("\n") }],
        details: cloneState(state),
      };
    },
  });

  pi.registerTool({
    name: "team_execution_note",
    label: "Team Execution Note",
    description: "Record whether Pi is actively using the team, paused, or temporarily working solo, including a visible reason.",
    promptSnippet: "Record a team execution transition such as active, paused, solo fallback, or done.",
    promptGuidelines: [
      "When you stop reusing an active team and continue solo, call this tool with mode='solo_fallback' and include a concise reason before proceeding.",
      "When you resume team execution, call this tool with mode='active'.",
    ],
    parameters: Type.Object({
      mode: Type.String({ description: "One of: active, paused, solo_fallback, done" }),
      reason: Type.String({ description: "Concise explanation for the current mode." }),
      teamId: Type.Optional(Type.String({ description: "Optional team id if known." })),
    }),
    async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
      const allowed: TeamExecutionMode[] = ["active", "paused", "solo_fallback", "done"];
      if (!allowed.includes(params.mode as TeamExecutionMode)) {
        return {
          content: [{ type: "text", text: `Unsupported mode: ${params.mode}` }],
          details: cloneState(state),
          isError: true,
        };
      }

      state = mergeState(state, {
        mode: params.mode as TeamExecutionMode,
        teamId: params.teamId ?? state.teamId,
        lastReason: params.reason.trim(),
        observabilityAttached: true,
        continueUsingTeamByDefault: params.mode !== "done",
        activeSince: params.mode === "active" ? state.activeSince ?? new Date().toISOString() : state.activeSince,
      });
      persistState();
      renderStatus(ctx);

      return {
        content: [{ type: "text", text: `Recorded team execution mode: ${state.mode}` }],
        details: cloneState(state),
      };
    },
  });

  pi.registerCommand("team-execution-status", {
    description: "Show the current team execution mode and fallback reason",
    handler: async (_args, ctx) => {
      renderStatus(ctx);
      const view = buildStatusView(state);
      ctx.ui.notify(`${view.label || "team: inactive"}\n${view.summary}`, "info");
    },
  });

  pi.on("session_start", async (_event, ctx) => {
    state = createDefaultState();
    for (const entry of ctx.sessionManager.getEntries()) {
      if (entry.type !== "custom" || entry.customType !== ENTRY_TYPE || !isStateLike(entry.data)) continue;
      state = mergeState(state, entry.data as Partial<TeamExecutionState>);
    }
    renderStatus(ctx);
  });

  pi.on("tool_execution_end", async (event, ctx) => {
    if (event.toolName !== "teams") return;

    const details = extractToolDetails(event.result);
    const next = deriveStateFromTeamsTool(state, details, new Date().toISOString());
    const changed = JSON.stringify(next) !== JSON.stringify(state);
    state = next;
    if (changed) persistState();
    renderStatus(ctx);
  });

  pi.on("before_agent_start", async (event) => {
    const extra = buildExecutionInstructions(state, event.prompt);
    if (!extra) return;

    return {
      systemPrompt: `${event.systemPrompt}\n\n${extra}`,
    };
  });
}
