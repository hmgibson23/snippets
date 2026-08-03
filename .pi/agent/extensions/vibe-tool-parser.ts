/**
 * Vibe Model XML Tool Call Parser
 *
 * Intercepts responses from the vibe (VibeThinker-3B) model and converts
 * XML-wrapped function calls in text content to proper tool_calls format.
 *
 * The model outputs tool calls as JSON wrapped in various XML tags.
 * Known formats:
 *   <prefix>{"name": "fn", "arguments": {...}}</prefix>
 *   <tool_call>{"name": "fn", "arguments": {...}}</tool_call>
 *   <tag_response name="fn">{...}</tag_response>
 *   <command>{"name": "fn", "arguments": {...}}</command>
 *   <pm>{"name": "fn", "arguments": {...}}</pm>
 *   <brackets>{"name": "fn", "arguments": {...}}</brackets>
 *
 * Strategy: match ANY XML tag wrapping JSON that contains "name" and "arguments".
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { ToolCall } from "@earendil-works/pi-ai";

export default function (pi: ExtensionAPI) {
	pi.on("message_end", (event) => {
		const message = event.message;
		if (message.role !== "assistant") return;

		// Only handle the vibe model
		if (message.provider !== "vibe-local" || message.model !== "vibe") {
			// Also handle from the 'local' provider (main server)
			if (message.provider !== "local" || message.model !== "vibe") return;
		}

		// Find text blocks in content
		const textBlocks = message.content.filter(
			(c): c is { type: "text"; text: string } => c.type === "text",
		);
		if (textBlocks.length === 0) return;

		const toolCalls: ToolCall[] = [];
		let textModified = false;

		const newContent = message.content
			.map((block) => {
				if (block.type !== "text") return block;

				const result = parseToolCallsFromText(block.text, toolCalls);
				if (!result.modified) return block;

				textModified = true;
				if (result.cleanText) {
					return { type: "text" as const, text: result.cleanText };
				}
				return null; // Remove empty text blocks
			})
			.filter(Boolean) as (
				| ToolCall
				| { type: "text"; text: string }
				| { type: "thinking"; thinking: string }
			)[];

		if (!textModified) return;

		// Append parsed tool calls to content
		newContent.push(...toolCalls);

		return {
			message: {
				...message,
				content: newContent,
				stopReason: toolCalls.length > 0 ? "toolUse" : message.stopReason,
			},
		};
	});
}

interface ParseResult {
	modified: boolean;
	cleanText: string;
}

/**
 * Parse XML-wrapped tool call JSON from text content.
 * Matches any XML tag pair wrapping a JSON object with "name" and "arguments" fields.
 *
 * Matches patterns like:
 *   <anything>{"name": "fn", "arguments": {...}}</anything>
 *   <anything>{"name": "fn", "arguments": {...}}}</anything>  (extra brace)
 *   <anything name="fn">{...}</anything>  (name from attribute)
 */
function parseToolCallsFromText(text: string, toolCalls: ToolCall[]): ParseResult {
	// Strategy 1: Match tools wrapped in ANY XML tag pair
	// Capture opening tag name, optional attributes, and content
	const pairPattern = /<(\w+)([^>]*)>\s*(\{.*?\})\s*<\/\1>/gs;

	let match;
	let lastIndex = 0;
	const parts: string[] = [];
	let modified = false;

	while ((match = pairPattern.exec(text)) !== null) {
		// Add text before this tag
		parts.push(text.slice(lastIndex, match.index));

		const tagName = match[1];
		const tagAttrs = match[2];
		const jsonContent = match[3];

		if (tryExtractToolCall(jsonContent, tagName, tagAttrs, toolCalls)) {
			modified = true;
		} else {
			// Couldn't parse, keep original text
			parts.push(match[0]);
		}

		lastIndex = match.index + match[0].length;
	}

	// Add remaining text
	parts.push(text.slice(lastIndex));

	const cleanText = parts.join("").trim();
	return { modified, cleanText };
}

/**
 * Try to extract a tool call from JSON content inside XML tags.
 * Handles multiple formatting styles.
 */
function tryExtractToolCall(
	jsonContent: string,
	tagName: string,
	tagAttrs: string,
	toolCalls: ToolCall[],
): boolean {
	try {
		// Try to parse the JSON content directly
		let parsed = tryParseJson(jsonContent);
		if (!parsed) {
			// If the model outputs <tag><{JSON}></tag> (extra angle brackets)
			const innerMatch = jsonContent.match(/^\s*<\s*(\{.*?\})\s*>\s*$/s);
			if (innerMatch) {
				parsed = tryParseJson(innerMatch[1]);
			}
		}

		if (!parsed) return false;

		let name: string | undefined;
		let args: Record<string, unknown> | undefined;

		// Try to get name from JSON
		if (typeof parsed.name === "string") {
			name = parsed.name;
		}

		// Try to get name from XML attribute (e.g., <tag_response name="read">)
		if (!name) {
			const attrMatch = tagAttrs.match(/name\s*=\s*"([^"]+)"/);
			if (attrMatch) {
				name = attrMatch[1];
			}
		}

		// Get arguments
		if (parsed.arguments !== undefined) {
			args = typeof parsed.arguments === "string"
				? tryParseJson(parsed.arguments) || ({} as Record<string, unknown>)
				: (parsed.arguments as Record<string, unknown>);
		} else if (tagName === "tag_response" && name) {
			// <tag_response name="fn">{...}</tag_response> — the JSON IS the arguments
			args = parsed as Record<string, unknown>;
		}

		if (name && args && typeof args === "object") {
			toolCalls.push({
				type: "toolCall",
				id: `call_${Date.now()}_${toolCalls.length}_${Math.random().toString(36).slice(2, 6)}`,
				name,
				arguments: args,
			});
			return true;
		}

		return false;
	} catch {
		return false;
	}
}

/**
 * Try to parse a JSON string. Returns the parsed object or null.
 */
function tryParseJson(str: string): Record<string, unknown> | null {
	try {
		const result = JSON.parse(str.trim());
		if (typeof result === "object" && result !== null) {
			return result as Record<string, unknown>;
		}
		return null;
	} catch {
		// Try to fix common issues: trailing comma, single quotes, etc.
		try {
			// Handle extra closing brace (some models output }} instead of })
			let fixed = str.trim();
			const openBraces = (fixed.match(/\{/g) || []).length;
			const closeBraces = (fixed.match(/\}/g) || []).length;
			if (closeBraces > openBraces) {
				// Remove trailing braces
				const extra = closeBraces - openBraces;
				for (let i = 0; i < fixed.length && extra > 0; i++) {
					const idx = fixed.length - 1 - i;
					if (fixed[idx] === "}") {
						fixed = fixed.slice(0, idx) + fixed.slice(idx + 1);
					}
				}
			}
			const result = JSON.parse(fixed);
			if (typeof result === "object" && result !== null) {
				return result as Record<string, unknown>;
			}
		} catch {
			// Not parseable
		}
		return null;
	}
}
