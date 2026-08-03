import test from "node:test";
import assert from "node:assert/strict";
import {
  buildExecutionInstructions,
  buildStatusView,
  createDefaultState,
  deriveStateFromTeamsTool,
} from "./logic";

test("delegate action activates team execution and observability", () => {
  const initial = createDefaultState();
  const next = deriveStateFromTeamsTool(initial, { action: "delegate", teamId: "team-123" }, "2026-04-17T12:00:00.000Z");

  assert.equal(next.mode, "active");
  assert.equal(next.teamId, "team-123");
  assert.equal(next.observabilityAttached, true);
  assert.match(next.lastReason ?? "", /continue reusing teammates/i);
});

test("team_done marks the team run ended", () => {
  const initial = deriveStateFromTeamsTool(createDefaultState(), { action: "delegate", teamId: "team-123" }, "2026-04-17T12:00:00.000Z");
  const next = deriveStateFromTeamsTool(initial, { action: "team_done", teamId: "team-123" }, "2026-04-17T12:05:00.000Z");

  assert.equal(next.mode, "done");
  assert.equal(buildStatusView(next).label, "✓ team: ended");
});

test("member_shutdown all pauses team execution", () => {
  const initial = deriveStateFromTeamsTool(createDefaultState(), { action: "delegate", teamId: "team-123" }, "2026-04-17T12:00:00.000Z");
  const next = deriveStateFromTeamsTool(initial, { action: "member_shutdown", teamId: "team-123", all: true }, "2026-04-17T12:05:00.000Z");

  assert.equal(next.mode, "paused");
  assert.equal(buildStatusView(next).label, "⏸ team: paused");
});

test("active team prompt asks pi to inspect member_status for delay questions", () => {
  const state = deriveStateFromTeamsTool(createDefaultState(), { action: "delegate", teamId: "team-123" }, "2026-04-17T12:00:00.000Z");
  const instructions = buildExecutionInstructions(state, "why is this taking so long?");

  assert.match(instructions, /member_status/);
  assert.match(instructions, /gentle steer/i);
  assert.match(instructions, /solo_fallback/);
});

test("inactive team execution does not inject extra instructions", () => {
  assert.equal(buildExecutionInstructions(createDefaultState(), "why is this taking so long?"), "");
});
