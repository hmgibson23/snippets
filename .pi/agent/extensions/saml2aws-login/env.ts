const REQUIRED_AWS_KEYS = ["AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN"] as const;

export interface AwsCallerIdentity {
  account: string;
  arn?: string;
}

export function parseEnvScript(text: string): Record<string, string> {
  const env: Record<string, string> = {};

  for (const rawLine of text.split(/\r?\n/)) {
    let line = rawLine.trim();
    if (!line || line.startsWith("#")) continue;

    if (line.startsWith("export ")) {
      line = line.slice("export ".length).trim();
    }

    const equalsIndex = line.indexOf("=");
    if (equalsIndex === -1) continue;

    const key = line.slice(0, equalsIndex).trim();
    let value = line.slice(equalsIndex + 1).trim();

    const isDoubleQuoted = value.startsWith('"') && value.endsWith('"');
    const isSingleQuoted = value.startsWith("'") && value.endsWith("'");
    if (isDoubleQuoted || isSingleQuoted) {
      value = value.slice(1, -1);
    }

    env[key] = value;
  }

  for (const key of REQUIRED_AWS_KEYS) {
    if (!env[key]) {
      throw new Error(`Missing required AWS variable: ${key}`);
    }
  }

  return env;
}

export function assertCredentialsNotExpired(vars: Record<string, string>, now: Date = new Date()): void {
  const expiration = vars.AWS_CREDENTIAL_EXPIRATION;
  if (!expiration) {
    return;
  }

  const expiresAt = new Date(expiration);
  if (Number.isNaN(expiresAt.getTime())) {
    throw new Error(`Invalid AWS_CREDENTIAL_EXPIRATION: ${expiration}`);
  }

  if (expiresAt.getTime() <= now.getTime()) {
    throw new Error(`AWS credentials are expired as of ${expiration}`);
  }
}

export function parseCallerIdentity(text: string): AwsCallerIdentity {
  const parsed = JSON.parse(text) as { Account?: unknown; Arn?: unknown };
  const account = typeof parsed.Account === "string" ? parsed.Account.trim() : "";
  if (!account) {
    throw new Error("Missing AWS account id in caller identity output");
  }

  const arn = typeof parsed.Arn === "string" && parsed.Arn.trim() ? parsed.Arn : undefined;
  return arn ? { account, arn } : { account };
}

export function formatAwsIdentity(input: { profile: string; account?: string; region?: string }): string {
  const profileAndAccount = input.account ? `${input.profile} (${input.account})` : input.profile;
  return input.region ? `${profileAndAccount} ${input.region}` : profileAndAccount;
}

export function applyEnvVars(vars: Record<string, string>): void {
  for (const [key, value] of Object.entries(vars)) {
    process.env[key] = value;
  }
}
