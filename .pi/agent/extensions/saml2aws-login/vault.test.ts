import test from "node:test";
import assert from "node:assert/strict";
import { assertNotExpired, redact, scrubSecretKeys } from "./vault";
import { runWithCredentials } from "./exec";

const creds = {
  AWS_ACCESS_KEY_ID: "ASIAFAKEKEY12345678",
  AWS_SECRET_ACCESS_KEY: "fakeSecret/with+chars1234567890abcdef",
  AWS_SESSION_TOKEN: "fakeTokenAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
};

test("redact masks raw, url-encoded and base64 forms", () => {
  const text = [
    creds.AWS_SECRET_ACCESS_KEY,
    encodeURIComponent(creds.AWS_SECRET_ACCESS_KEY),
    Buffer.from(creds.AWS_SESSION_TOKEN).toString("base64"),
    creds.AWS_ACCESS_KEY_ID,
  ].join(" | ");
  const out = redact(text, creds);
  for (const v of Object.values(creds)) assert.ok(!out.includes(v));
  assert.ok(out.includes("[REDACTED]"));
});

test("redact leaves unrelated text alone", () => {
  assert.equal(redact("hello world", creds), "hello world");
});

test("assertNotExpired throws for past expiry only", () => {
  assert.throws(() => assertNotExpired({ AWS_CREDENTIAL_EXPIRATION: "2000-01-01T00:00:00Z" }));
  assert.doesNotThrow(() => assertNotExpired({ AWS_CREDENTIAL_EXPIRATION: "2999-01-01T00:00:00Z" }));
  assert.doesNotThrow(() => assertNotExpired({}));
});

test("scrubSecretKeys removes credential vars", () => {
  const env: Record<string, string | undefined> = { ...creds, AWS_REGION: "eu-west-2", AWS_CREDENTIAL_EXPIRATION: "x" };
  scrubSecretKeys(env);
  assert.deepEqual(Object.keys(env), ["AWS_REGION"]);
});

test("exec injects creds into the child, redacts output, and ignores inherited creds", async () => {
  const r = await runWithCredentials('echo "$AWS_SECRET_ACCESS_KEY"; echo "key=$AWS_ACCESS_KEY_ID home=$HOME"', creds, {
    timeoutMs: 5000,
    baseEnv: { PATH: process.env.PATH, HOME: "/tmp", AWS_ACCESS_KEY_ID: "STALEINHERITED0000" },
  });
  assert.equal(r.exitCode, 0);
  assert.ok(!r.output.includes(creds.AWS_SECRET_ACCESS_KEY));
  assert.ok(!r.output.includes("STALEINHERITED"));
  assert.ok(r.output.includes("[REDACTED]"));
  assert.ok(r.output.includes("home=/tmp"));
});

test("exec reports non-zero exit and times out", async () => {
  const bad = await runWithCredentials("exit 3", creds, { timeoutMs: 5000 });
  assert.equal(bad.exitCode, 3);
  const slow = await runWithCredentials("sleep 5", creds, { timeoutMs: 200 });
  assert.equal(slow.timedOut, true);
});

test("exec refuses expired credentials", async () => {
  await assert.rejects(
    runWithCredentials("true", { ...creds, AWS_CREDENTIAL_EXPIRATION: "2000-01-01T00:00:00Z" }, { timeoutMs: 1000 }),
    /expired/,
  );
});
