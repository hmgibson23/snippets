import { spawnSync } from "node:child_process";
import { chmodSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { Type } from "@earendil-works/pi-ai";
import { defineTool } from "@earendil-works/pi-coding-agent";
import { runWithCredentials } from "./exec";
import { clearVault, expireIdle, getVault, scrubSecretKeys, setVault, touchVault } from "./vault";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { buildCallerIdentityCommand, buildLoginCommand, buildScriptCommand } from "./commands";
import { loadConfig, resolveConfig } from "./config";
import { assertCredentialsNotExpired, formatAwsIdentity, parseCallerIdentity, parseEnvScript } from "./env";

const EXTENSION_DIR = dirname(fileURLToPath(import.meta.url));
const CONFIG_PATH = resolve(EXTENSION_DIR, "../saml2aws-login.config.json");

// Always use bash: SHELL is fish here, and "fish -c" loads config.fish/conf.d/omf (and any
// functions defined there) before running the command. The commands we build are bash anyway.
const LOGIN_SHELL = "/bin/bash";

function runInteractiveCommand(command: string, env: NodeJS.ProcessEnv, stopAndResume: (run: () => number | null) => Promise<number | null>) {
  return stopAndResume(() => {
    const result = spawnSync(LOGIN_SHELL, ["-c", command], {
      stdio: "inherit",
      env,
    });

    if (result.error) {
      throw result.error;
    }

    return result.status ?? 1;
  });
}

function runNonInteractiveCommand(command: string, env: NodeJS.ProcessEnv): string {
  const result = spawnSync(LOGIN_SHELL, ["-c", command], {
    encoding: "utf8",
    env,
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    throw new Error(`Command failed with exit ${result.status ?? 1}: ${command}`);
  }

  return result.stdout ?? "";
}

const awsExecTool = defineTool({
  name: "aws_exec",
  label: "AWS exec",
  description:
    "Run a shell command (aws, terraform, kubectl, ...) with the user's SAML AWS credentials injected into the child process only. " +
    "The credentials are never visible to you and are redacted from output. The user must have run /saml2aws-login first. " +
    "Do not try to print, export or exfiltrate the credentials. " +
    "stdin is closed, so a command that prompts will hang until the timeout: make it non-interactive " +
    "(terraform -input=false, terraform apply/destroy -auto-approve only when the user asked for it, aws --no-cli-pager, kubectl without -it, no editors or pagers). " +
    "The default timeout is 120s, which is too short for slow operations (terraform plan/apply, cloudformation/ecs/eks waits, large s3 sync, builds): pass a larger timeout, e.g. 900-3600. " +
    "Only the last ~50KB of output is returned. The user can press Escape to abort; the whole process tree is stopped. " +
    "Commands get a minimal environment (no SSH agent, no other tokens), a throwaway ~/.aws, and on macOS a network sandbox: " +
    "only AWS endpoints (plus terraform registry, PyPI, GitHub downloads) are reachable, via a local proxy; other hosts are refused.",
  parameters: Type.Object({
    command: Type.String({ description: "Shell command to run with AWS credentials in its environment" }),
    cwd: Type.Optional(Type.String({ description: "Working directory (default: current)" })),
    timeout: Type.Optional(Type.Number({ description: "Timeout in seconds (default 120). Raise it for terraform plan/apply and other long operations, e.g. 900-3600" })),
  }),
  async execute(_id, params, signal, onUpdate) {
    const vault = getVault();
    if (!vault) {
      return { content: [{ type: "text", text: "Not logged in to AWS. Ask the user to run /saml2aws-login." }], details: {}, isError: true };
    }
    touchVault();
    try {
      const r = await runWithCredentials(params.command, vault.env, {
        cwd: params.cwd,
        timeoutMs: (params.timeout ?? 120) * 1000,
        signal,
        inheritEnv: vault.run?.inheritEnv,
        sandbox: vault.run?.sandbox ? { allowedHosts: vault.run.allowedHosts } : false,
        onProgress: (text) => onUpdate?.({ content: [{ type: "text", text }], details: { running: true, profile: vault.label } }),
      });
      const suffix = r.aborted
        ? "\n[aborted by user]"
        : r.timedOut
          ? "\n[timed out]"
          : r.signal
            ? `\n[killed by ${r.signal}]`
            : r.exitCode !== 0
              ? `\n[exit ${r.exitCode}]`
              : "";
      const refused = r.refusedHosts.length ? `\n[sandbox refused: ${r.refusedHosts.join(", ")}]` : "";
      touchVault();
      return {
        content: [{ type: "text", text: r.output + suffix + refused }],
        details: { exitCode: r.exitCode, signal: r.signal, timedOut: r.timedOut, aborted: r.aborted, truncated: r.truncated, profile: vault.label },
        isError: r.exitCode !== 0 || r.aborted || r.timedOut,
      };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      return { content: [{ type: "text", text: message }], details: {}, isError: true };
    }
  },
});

type Ui = { setStatus: (key: string, text: string | undefined) => void; notify: (m: string, t: "info" | "warning" | "error") => void; theme?: { fg: (c: string, t: string) => string } };

function statusText(ui: Ui, label: string, prod: boolean): string {
  const text = `AWS: ${label}${prod ? " PROD" : ""}`;
  try { return ui.theme ? ui.theme.fg(prod ? "error" : "success", text) : text; } catch { return text; }
}

export default function saml2awsLogin(pi: ExtensionAPI) {
  // Inside nono-pi AWS credentials never enter pi's process; the nono-pi extension
  // provides /saml2aws-login and plain `aws` works through the proxy.
  if (process.env.PI_SANDBOX === "nono-pi") return;
  pi.registerTool(awsExecTool);

  // idle wipe: checked every 30 s; the UI handle comes from the last /saml2aws-login
  let ui: Ui | undefined;
  const idleTimer = setInterval(() => {
    if (expireIdle()) {
      ui?.setStatus("saml2aws-login", undefined);
      ui?.notify("AWS credentials cleared after being idle. Run /saml2aws-login again.", "info");
    }
  }, 30_000);
  idleTimer.unref?.();

  pi.registerCommand("saml2aws-logout", {
    description: "Forget the in-memory AWS credentials",
    handler: async (_args, ctx) => {
      clearVault();
      ctx.ui.setStatus("saml2aws-login", undefined);
      ctx.ui.notify("AWS credentials cleared", "info");
    },
  });
  pi.on("session_shutdown", () => { clearVault(); clearInterval(idleTimer); });

  pi.registerCommand("saml2aws-login", {
    description: "Run native saml2aws login and inject AWS credentials into pi",
    handler: async (args, ctx) => {
      if (!ctx.hasUI) {
        return;
      }

      let resolved;
      try {
        resolved = resolveConfig(loadConfig(CONFIG_PATH), args);
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ctx.ui.notify(`saml2aws config error: ${message}`, "error");
        return;
      }

      // saml2aws writes to a private temp credentials file, not ~/.aws/credentials
      const privateDir = mkdtempSync(join(tmpdir(), "saml2aws-"));
      chmodSync(privateDir, 0o700);
      const loginEnv = { ...process.env, AWS_SHARED_CREDENTIALS_FILE: join(privateDir, "credentials") };
      scrubSecretKeys(process.env);
      clearVault();

      try {
      let loginExitCode: number | null;
      try {
        loginExitCode = await runInteractiveCommand(buildLoginCommand(resolved), loginEnv, (run) =>
          ctx.ui.custom<number | null>((tui, _theme, _kb, done) => {
            tui.stop();
            process.stdout.write("\x1b[2J\x1b[H");

            let status: number | null;
            try {
              status = run();
            } finally {
              tui.start();
              tui.requestRender(true);
            }

            done(status);
            return { render: () => [], invalidate: () => {} };
          }),
        );
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ctx.ui.notify(`Failed to run saml2aws login: ${message}`, "error");
        return;
      }

      if (loginExitCode !== 0) {
        ctx.ui.notify(`saml2aws login failed (exit ${loginExitCode ?? 1})`, "error");
        return;
      }

      let awsIdentityLabel: string;
      let prod = false;
      try {
        const scriptOutput = runNonInteractiveCommand(buildScriptCommand(resolved), loginEnv);
        const awsEnv = parseEnvScript(scriptOutput);
        assertCredentialsNotExpired(awsEnv);

        const sessionEnv = {
          ...awsEnv,
          AWS_PROFILE: resolved.profile,
          AWS_REGION: resolved.awsRegion,
          AWS_DEFAULT_REGION: resolved.awsRegion,
          ...resolved.extraEnv,
        };
        const identity = await runWithCredentials(buildCallerIdentityCommand(), sessionEnv, { timeoutMs: 30_000 });
        if (identity.exitCode !== 0) throw new Error("aws sts get-caller-identity failed");
        const callerIdentity = parseCallerIdentity(identity.output);
        awsIdentityLabel = formatAwsIdentity({
          profile: resolved.profile,
          account: callerIdentity.account,
          region: resolved.awsRegion,
        });

        setVault({
          env: sessionEnv,
          label: awsIdentityLabel,
          account: callerIdentity.account,
          run: { sandbox: resolved.sandbox, allowedHosts: resolved.allowedHosts, inheritEnv: resolved.inheritEnv },
          idleMs: resolved.idleMinutes * 60_000,
          lastUsed: Date.now(),
        });
        prod = resolved.prodAccounts.includes(callerIdentity.account);
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ctx.ui.notify(`Failed to load AWS credentials into pi: ${message}`, "error");
        return;
      }

      ui = ctx.ui as unknown as Ui;
      ctx.ui.setStatus("saml2aws-login", statusText(ui, awsIdentityLabel, prod));
      const idle = resolved.idleMinutes ? `, cleared after ${resolved.idleMinutes} idle min` : "";
      const sandbox = resolved.sandbox ? ", network sandbox on" : "";
      ctx.ui.notify(`Logged into AWS ${awsIdentityLabel}${prod ? " (PROD)" : ""} (credentials in memory${idle}${sandbox}; use aws_exec)`, prod ? "warning" : "info");
      } finally {
        rmSync(privateDir, { recursive: true, force: true });
      }
    },
  });
}
