import test from "node:test";
import assert from "node:assert/strict";
import { resolveConfig } from "./config";

test("resolveConfig uses default profile when no args are supplied", () => {
  const resolved = resolveConfig(
    {
      defaultProfile: "nuk-digital-dev-architecture",
      sessionDuration: 3600,
      awsRegion: "us-east-1",
      extraEnv: { CLAUDE_CODE_USE_BEDROCK: "1" },
    },
    "",
  );

  assert.equal(resolved.profile, "nuk-digital-dev-architecture");
});

test("resolveConfig lets command args override profile", () => {
  const resolved = resolveConfig(
    {
      defaultProfile: "default-profile",
      sessionDuration: 3600,
      awsRegion: "us-east-1",
      extraEnv: {},
    },
    "other-profile",
  );

  assert.equal(resolved.profile, "other-profile");
});

test("resolveConfig trims whitespace around args", () => {
  const resolved = resolveConfig(
    {
      defaultProfile: "default-profile",
      sessionDuration: 3600,
      awsRegion: "us-east-1",
      extraEnv: {},
    },
    "  explicit-profile  ",
  );

  assert.equal(resolved.profile, "explicit-profile");
});
