import { existsSync } from "node:fs";
import { dirname, join } from "node:path";

export function findProjectNapkinVault(startDir: string): string | undefined {
  let current = startDir;

  while (true) {
    if (existsSync(join(current, ".napkin"))) {
      return current;
    }

    const parent = dirname(current);
    if (parent === current) {
      return undefined;
    }
    current = parent;
  }
}

export function slugify(value: string): string {
  return value
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .replace(/-{2,}/g, "-");
}

export function buildDecisionMemory(input: {
  date: string;
  title: string;
  summary: string;
  rationale: string;
  evidence: string;
  files?: string[];
}): { relativePath: string; content: string } {
  const slug = slugify(input.title);
  const relativePath = `decisions/${input.date}-${slug}.md`;
  const fileLines = (input.files ?? []).length
    ? (input.files ?? []).map((file) => `- \`${file}\``).join("\n")
    : "- None recorded";

  const content = [
    "---",
    "type: decision",
    `date: ${input.date}`,
    "status: accepted",
    "source: pi-auto",
    "---",
    "",
    `# ${input.title}`,
    "",
    "## Decision",
    "",
    input.summary.trim(),
    "",
    "## Why",
    "",
    input.rationale.trim(),
    "",
    "## Evidence",
    "",
    input.evidence.trim(),
    "",
    "## Related files/notes",
    "",
    fileLines,
    "",
  ].join("\n");

  return { relativePath, content };
}

export function appendDiscoveryEntry(
  existingContent: string,
  input: {
    date: string;
    title: string;
    summary: string;
    evidence: string;
    files?: string[];
  },
): { nextContent: string; didAppend: boolean } {
  const normalized = existingContent.trim();
  const header = "# Discoveries";
  const dateHeading = `## ${input.date}`;
  const trimmedEvidence = input.evidence.trim().replace(/[.]+$/, "");
  const files = (input.files ?? []).length ? ` Files: ${(input.files ?? []).map((file) => `\`${file}\``).join(", ")}` : "";
  const entry = `- **${input.title}**: ${input.summary.trim()} Evidence: ${trimmedEvidence}.${files}`;

  if (normalized.includes(entry)) {
    return { nextContent: ensureTrailingNewline(existingContent || `${header}\n`), didAppend: false };
  }

  if (!normalized) {
    return {
      nextContent: `${header}\n\n${dateHeading}\n\n${entry}\n`,
      didAppend: true,
    };
  }

  if (normalized.includes(`${dateHeading}\n`)) {
    return {
      nextContent: ensureTrailingNewline(existingContent).replace(`${dateHeading}\n`, `${dateHeading}\n\n${entry}\n`),
      didAppend: true,
    };
  }

  return {
    nextContent: `${ensureTrailingNewline(existingContent)}\n${dateHeading}\n\n${entry}\n`,
    didAppend: true,
  };
}

function ensureTrailingNewline(value: string): string {
  return value.endsWith("\n") ? value : `${value}\n`;
}
