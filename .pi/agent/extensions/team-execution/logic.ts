export type TeamExecutionMode = "inactive" | "active" | "paused" | "solo_fallback" | "done";

export interface TeamExecutionState {
  mode: TeamExecutionMode;
  teamId?: string;
  activeSince?: string;
  lastTeamAction?: string;
  lastTeamActionAt?: string;
  lastReason?: string;
  continueUsingTeamByDefault: boolean;
  observabilityAttached: boolean;
}

export interface TeamsToolDetailsLike {
  action?: unknown;
  teamId?: unknown;
  all?: unknown;
}

export interface TeamExecutionStatusView {
  label: string;
  summary: string;
}

export function createDefaultState(): TeamExecutionState {
  return {
    mode: "inactive",
    continueUsingTeamByDefault: true,
    observabilityAttached: false,
  };
}

export function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

export function extractToolDetails(result: unknown): Record<string, unknown> | undefined {
  if (!isRecord(result)) return undefined;
  const details = result.details;
  return isRecord(details) ? details : undefined;
}

function asString(value: unknown): string | undefined {
  return typeof value === "string" && value.trim().length > 0 ? value.trim() : undefined;
}

function asBoolean(value: unknown): boolean | undefined {
  return typeof value === "boolean" ? value : undefined;
}

export function deriveStateFromTeamsTool(
  current: TeamExecutionState,
  details: TeamsToolDetailsLike | undefined,
  nowIso: string,
): TeamExecutionState {
  const next = { ...current };
  const action = asString(details?.action);
  const teamId = asString(details?.teamId) ?? current.teamId;

  if (!action) return next;

  next.teamId = teamId;
  next.lastTeamAction = action;
  next.lastTeamActionAt = nowIso;

  if (action === "delegate" || action === "member_spawn" || action === "task_assign") {
    next.mode = "active";
    next.observabilityAttached = true;
    next.continueUsingTeamByDefault = true;
    next.activeSince = current.activeSince ?? nowIso;
    next.lastReason = "Team execution active; continue reusing teammates unless the remaining work is tiny or strictly sequential.";
    return next;
  }

  if (action === "team_done") {
    next.mode = "done";
    next.lastReason = "Team run ended.";
    return next;
  }

  if (action === "member_shutdown" && asBoolean(details?.all) === true) {
    next.mode = "paused";
    next.lastReason = "All teammates were asked to shut down; team execution is paused.";
    return next;
  }

  return next;
}

export function buildStatusView(state: TeamExecutionState): TeamExecutionStatusView {
  switch (state.mode) {
    case "active":
      return {
        label: "👥 team: active",
        summary: state.lastReason ?? "Team execution is active.",
      };
    case "paused":
      return {
        label: "⏸ team: paused",
        summary: state.lastReason ?? "Team execution is paused.",
      };
    case "solo_fallback":
      return {
        label: "👤 team: solo fallback",
        summary: state.lastReason ?? "Pi is temporarily working solo instead of reusing the team.",
      };
    case "done":
      return {
        label: "✓ team: ended",
        summary: state.lastReason ?? "Team execution has ended.",
      };
    default:
      return {
        label: "",
        summary: "No active team execution context.",
      };
  }
}

function isObservabilityQuestion(prompt: string): boolean {
  const normalized = prompt.toLowerCase();
  return [
    "why is this taking so long",
    "taking so long",
    "what is",
    "who is",
    "stuck",
    "slow",
    "status",
    "why aren't we using the team",
    "why arent we using the team",
    "why did we stop using the team",
    "why did you stop using the team",
  ].some((fragment) => normalized.includes(fragment));
}

export function buildExecutionInstructions(state: TeamExecutionState, userPrompt: string): string {
  if (state.mode === "inactive") return "";

  const lines = [
    "[TEAM EXECUTION CONTEXT]",
    `Current mode: ${state.mode}.`,
    "Prefer continued use of the existing team for subsequent plan tasks when work remains substantial or parallelizable.",
    "Do not silently stop using teammates after the first delegation wave.",
    "If you switch to solo execution, first call the team_execution_note tool with mode='solo_fallback' and a concise reason, then explain that reason inline to the user.",
    "If you resume team execution later, call team_execution_note with mode='active'.",
    "If the current state is unclear, inspect teammates before making coordination decisions.",
    "Safe automatic actions: status refresh and gentle steer. Ask before wrap-up, reassignment, or ending team use.",
  ];

  if (isObservabilityQuestion(userPrompt)) {
    lines.push(
      "The user is asking for team observability. Use the teams tool action 'member_status' before answering, and summarize the likely bottleneck in plain English.",
    );
  }

  return lines.join("\n");
}
