import test from "node:test";
import assert from "node:assert/strict";
import { buildCallerIdentityCommand, buildLoginCommand, buildScriptCommand } from "./commands";

test("buildLoginCommand includes force, session duration, and profile", () => {
  const command = buildLoginCommand({ profile: "dev", sessionDuration: 3600 });

  assert.match(command, /^saml2aws login /);
  assert.match(command, / --force/);
  assert.match(command, /--session-duration 3600/);
  assert.match(command, /--profile 'dev'$/);
});

test("buildScriptCommand requests bash shell output", () => {
  const command = buildScriptCommand({ profile: "dev" });

  assert.equal(command, "saml2aws script --profile 'dev' --shell=bash");
});

test("buildCallerIdentityCommand requests caller identity as JSON", () => {
  assert.equal(buildCallerIdentityCommand(), "aws sts get-caller-identity --output json");
});

test("buildLoginCommand safely quotes single quotes in profile", () => {
  const command = buildLoginCommand({ profile: "dev'ops", sessionDuration: 900 });

  assert.equal(command, "saml2aws login --session-duration 900 --force --profile 'dev'\"'\"'ops'");
});
