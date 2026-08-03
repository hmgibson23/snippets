export interface HandoffSummary {
  userMessages: string[];
  assistantMessages: string[];
  files: string[];
}

export interface HandoffPromptInput {
  goal: string;
  cwd: string;
  sessionFile?: string;
  summary: HandoffSummary;
}

const FILE_REFERENCE_PATTERN = /(?<![\w/:.-])(?:[A-Za-z0-9_.-]+\/)+(?:[A-Za-z0-9_.-]+)(?:\.[A-Za-z0-9_.-]+)?/g;

function textFromContent(content: unknown): string {
  if (typeof content === "string") {
    return content;
  }

  if (!Array.isArray(content)) {
    return "";
  }

  return content
    .map((part) => {
      if (typeof part === "string") return part;
      if (part && typeof part === "object" && "text" in part && typeof (part as { text?: unknown }).text === "string") {
        return (part as { text: string }).text;
      }
      return "";
    })
    .filter(Boolean)
    .join("\n");
}

function truncate(text: string, maxLength: number): string {
  const normalized = text.replace(/\s+/g, " ").trim();
  if (normalized.length <= maxLength) {
    return normalized;
  }
  return `${normalized.slice(0, Math.max(0, maxLength - 1)).trimEnd()}…`;
}

export function extractFileReferences(text: string): string[] {
  const refs = new Set<string>();
  for (const match of text.matchAll(FILE_REFERENCE_PATTERN)) {
    const path = match[0].replace(/[),.;:]+$/, "");
    if (path.startsWith("http/") || path.includes("://")) continue;
    refs.add(path);
  }
  return [...refs];
}

export function summarizeBranch(entries: unknown[], limits = { messages: 8, chars: 220, files: 12 }): HandoffSummary {
  const userMessages: string[] = [];
  const assistantMessages: string[] = [];
  const files = new Set<string>();

  for (const entry of entries) {
    if (!entry || typeof entry !== "object") continue;
    if ((entry as { type?: unknown }).type !== "message") continue;

    const message = (entry as { message?: unknown }).message;
    if (!message || typeof message !== "object") continue;

    const role = (message as { role?: unknown }).role;
    const text = textFromContent((message as { content?: unknown }).content);
    if (!text.trim()) continue;

    for (const file of extractFileReferences(text)) {
      files.add(file);
    }

    const line = truncate(text, limits.chars);
    if (role === "user") {
      userMessages.push(line);
    } else if (role === "assistant") {
      assistantMessages.push(line);
    }
  }

  return {
    userMessages: userMessages.slice(-limits.messages),
    assistantMessages: assistantMessages.slice(-limits.messages),
    files: [...files].slice(0, limits.files),
  };
}

function bulletList(items: string[], emptyText: string, formatter: (item: string) => string = (item) => item): string {
  if (items.length === 0) return `- ${emptyText}`;
  return items.map((item) => `- ${formatter(item)}`).join("\n");
}

export function buildHandoffPrompt(input: HandoffPromptInput): string {
  const goal = input.goal.trim() || "Continue from the current session.";
  const sessionLine = input.sessionFile ? `- session: \`${input.sessionFile}\`` : "- session: unknown";

  return [
    "# Handoff Pack",
    "",
    "## Goal",
    goal,
    "",
    "## Current context",
    `- cwd: \`${input.cwd}\``,
    sessionLine,
    "",
    "## Recent user requests",
    bulletList(input.summary.userMessages, "No recent user messages captured."),
    "",
    "## Recent assistant work",
    bulletList(input.summary.assistantMessages, "No recent assistant messages captured."),
    "",
    "## Files mentioned",
    bulletList(input.summary.files, "No file references captured.", (file) => `\`${file}\``),
    "",
    "## Next step",
    "Use this context to continue the task in a focused new session. Verify assumptions against the repository before editing.",
  ].join("\n");
}
