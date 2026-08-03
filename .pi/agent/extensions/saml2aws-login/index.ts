import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { buildCallerIdentityCommand, buildLoginCommand, buildScriptCommand } from "./commands";
import { loadConfig, resolveConfig } from "./config";
import { applyEnvVars, assertCredentialsNotExpired, formatAwsIdentity, parseCallerIdentity, parseEnvScript } from "./env";

const EXTENSION_DIR = dirname(fileURLToPath(import.meta.url));
const CONFIG_PATH = resolve(EXTENSION_DIR, "../saml2aws-login.config.json");

function runInteractiveCommand(command: string, env: NodeJS.ProcessEnv, stopAndResume: (run: () => number | null) => Promise<number | null>) {
  return stopAndResume(() => {
    const shell = process.env.SHELL || "/bin/sh";
    const result = spawnSync(shell, ["-c", command], {
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
  const shell = process.env.SHELL || "/bin/sh";
  const result = spawnSync(shell, ["-c", command], {
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

export default function saml2awsLogin(pi: ExtensionAPI) {
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

      let loginExitCode: number | null;
      try {
        loginExitCode = await runInteractiveCommand(buildLoginCommand(resolved), process.env, (run) =>
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
      try {
        const scriptOutput = runNonInteractiveCommand(buildScriptCommand(resolved), process.env);
        const awsEnv = parseEnvScript(scriptOutput);
        assertCredentialsNotExpired(awsEnv);

        const sessionEnv = {
          ...awsEnv,
          AWS_PROFILE: resolved.profile,
          AWS_REGION: resolved.awsRegion,
          AWS_DEFAULT_REGION: resolved.awsRegion,
          ...resolved.extraEnv,
        };
        const callerIdentityOutput = runNonInteractiveCommand(buildCallerIdentityCommand(), {
          ...process.env,
          ...sessionEnv,
        });
        const callerIdentity = parseCallerIdentity(callerIdentityOutput);
        awsIdentityLabel = formatAwsIdentity({
          profile: resolved.profile,
          account: callerIdentity.account,
          region: resolved.awsRegion,
        });

        applyEnvVars(sessionEnv);
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ctx.ui.notify(`Failed to load AWS credentials into pi: ${message}`, "error");
        return;
      }

      ctx.ui.setStatus("saml2aws-login", `AWS: ${awsIdentityLabel}`);
      ctx.ui.notify(`Logged into AWS ${awsIdentityLabel}`, "info");
    },
  });
}
