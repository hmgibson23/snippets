import path from "node:path";

const LEADING_FILLER = /^(please|can you|could you|would you|let'?s|help me|i need you to)\s+/i;
const SLASH_COMMAND = /^\/[\w:-]+\s+/;

export function titleFromPrompt(prompt: string, maxLength = 48): string {
  let title = prompt.replace(SLASH_COMMAND, "").replace(/\s+/g, " ").trim();
  title = title.replace(LEADING_FILLER, "").trim();

  if (!title) {
    return "session";
  }

  if (title.length <= maxLength) {
    return title;
  }

  return title.slice(0, Math.max(0, maxLength - 1)).trimEnd();
}

export function buildPiTitle(input: { task?: string; cwd: string }): string {
  const cwdName = path.basename(input.cwd) || input.cwd;
  return input.task ? `π - ${input.task} - ${cwdName}` : `π - ${cwdName}`;
}
