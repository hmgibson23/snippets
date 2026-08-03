function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'"'"'`)}'`;
}

export function buildLoginCommand(input: { profile: string; sessionDuration: number }): string {
  return [
    "saml2aws login",
    `--session-duration ${input.sessionDuration}`,
    "--force",
    `--profile ${shellQuote(input.profile)}`,
  ].join(" ");
}

export function buildScriptCommand(input: { profile: string }): string {
  return `saml2aws script --profile ${shellQuote(input.profile)} --shell=bash`;
}

export function buildCallerIdentityCommand(): string {
  return "aws sts get-caller-identity --output json";
}
