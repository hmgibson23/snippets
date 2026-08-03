import test from "node:test";
import assert from "node:assert/strict";
import { applyEnvVars, assertCredentialsNotExpired, formatAwsIdentity, parseCallerIdentity, parseEnvScript } from "./env";

test("parseEnvScript parses export KEY=value lines", () => {
  const parsed = parseEnvScript(
    [
      "export AWS_ACCESS_KEY_ID=abc",
      "export AWS_SECRET_ACCESS_KEY=def",
      "export AWS_SESSION_TOKEN=ghi",
      "export AWS_CREDENTIAL_EXPIRATION=2099-03-19T13:50:14Z",
    ].join("\n"),
  );

  assert.equal(parsed.AWS_ACCESS_KEY_ID, "abc");
  assert.equal(parsed.AWS_SECRET_ACCESS_KEY, "def");
  assert.equal(parsed.AWS_SESSION_TOKEN, "ghi");
  assert.equal(parsed.AWS_CREDENTIAL_EXPIRATION, "2099-03-19T13:50:14Z");
});

test("parseEnvScript strips surrounding quotes and ignores blanks", () => {
  const parsed = parseEnvScript(
    [
      "export AWS_ACCESS_KEY_ID=abc",
      'export AWS_SECRET_ACCESS_KEY="def"',
      "export AWS_SESSION_TOKEN='ghi'",
      "",
      'export AWS_PROFILE="dev"',
      "export AWS_REGION='us-east-1'",
    ].join("\n"),
  );

  assert.equal(parsed.AWS_PROFILE, "dev");
  assert.equal(parsed.AWS_REGION, "us-east-1");
  assert.equal(parsed.AWS_SECRET_ACCESS_KEY, "def");
  assert.equal(parsed.AWS_SESSION_TOKEN, "ghi");
});

test("applyEnvVars mutates process.env only for provided keys", () => {
  const original = process.env.AWS_PROFILE;
  applyEnvVars({ AWS_PROFILE: "nuk-digital-dev-architecture" });
  assert.equal(process.env.AWS_PROFILE, "nuk-digital-dev-architecture");

  if (original === undefined) delete process.env.AWS_PROFILE;
  else process.env.AWS_PROFILE = original;
});

test("parseEnvScript throws when required AWS keys are missing", () => {
  assert.throws(() => parseEnvScript("export AWS_PROFILE=dev"), /Missing required AWS variable/);
});

test("assertCredentialsNotExpired accepts future credential expiration", () => {
  assert.doesNotThrow(() =>
    assertCredentialsNotExpired(
      { AWS_CREDENTIAL_EXPIRATION: "2099-03-19T13:50:14Z" },
      new Date("2026-03-19T13:00:00Z"),
    ),
  );
});

test("assertCredentialsNotExpired rejects expired credentials", () => {
  assert.throws(
    () =>
      assertCredentialsNotExpired(
        { AWS_CREDENTIAL_EXPIRATION: "2026-03-19T13:50:14Z" },
        new Date("2026-03-19T13:50:15Z"),
      ),
    /expired/,
  );
});

test("parseCallerIdentity parses account id and arn from AWS STS JSON", () => {
  const identity = parseCallerIdentity(
    JSON.stringify({
      UserId: "AROATEST:person@example.com",
      Account: "123456789012",
      Arn: "arn:aws:sts::123456789012:assumed-role/Admin/person@example.com",
    }),
  );

  assert.deepEqual(identity, {
    account: "123456789012",
    arn: "arn:aws:sts::123456789012:assumed-role/Admin/person@example.com",
  });
});

test("parseCallerIdentity rejects STS output without an account id", () => {
  assert.throws(() => parseCallerIdentity(JSON.stringify({ Arn: "arn" })), /Missing AWS account id/);
});

test("formatAwsIdentity includes profile and account id", () => {
  assert.equal(
    formatAwsIdentity({ profile: "prod-admin", account: "123456789012" }),
    "prod-admin (123456789012)",
  );
});

test("formatAwsIdentity includes region when supplied", () => {
  assert.equal(
    formatAwsIdentity({ profile: "prod-admin", account: "123456789012", region: "eu-west-1" }),
    "prod-admin (123456789012) eu-west-1",
  );
});
