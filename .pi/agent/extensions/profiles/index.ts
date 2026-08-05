/**
 * Scoped Models Extension
 *
 * Automatically configures the scoped-models list (Ctrl+P cycling, "scoped" tab in model selector)
 * based on the current working directory. No model blocking — just smart defaults.
 *
 *   ~/git/newsuk/**  → scoped = github-copilot + local models only
 *   anywhere else     → scoped = all models *except* github-copilot
 *
 * Removed from old profiles extension:
 *   - NO model_select blocking (no more "profile:work blocks X" error)
 *   - NO before_provider_request guardrail
 *   - NO forced model switching on startup
 *
 * The scoped-models list is written to ~/.pi/agent/settings.json (enabledModels).
 * Changes take effect on the next session start. Use /reload to apply immediately.
 *
 * Config: ~/.pi/agent/profiles.json (same file as before, keeps backward compat)
 */

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve, sep } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { getAgentDir } from "@earendil-works/pi-coding-agent";

// ── Directory Rules ─────────────────────────────────────────────────────────

/** Providers considered "local" — always included in newsuk mode */
const LOCAL_PROVIDERS = new Set(["local", "local-coder", "llama-cpp", "vibe-local"]);

/** Provider to exclude from scoped when outside newsuk */
const WORK_PROVIDER = "github-copilot";

// ── Config (backward compat with profiles.json) ─────────────────────────────

interface Rule {
	match: string; // path (supports leading "~/") or "*"
	profile: string;
}

interface ProfilesFile {
	rules: Rule[];
}

const FALLBACK: ProfilesFile = {
	rules: [
		{ match: "~/git/newsuk", profile: "newsuk" },
		{ match: "*", profile: "other" },
	],
};

function expandHome(p: string): string {
	if (p === "~") return homedir();
	if (p.startsWith("~/")) return join(homedir(), p.slice(2));
	return p;
}

function loadRules(): Rule[] {
	const path = join(getAgentDir(), "profiles.json");
	if (!existsSync(path)) return FALLBACK.rules;
	try {
		const raw = JSON.parse(readFileSync(path, "utf-8")) as Partial<ProfilesFile>;
		return raw.rules && raw.rules.length > 0 ? raw.rules : FALLBACK.rules;
	} catch {
		return FALLBACK.rules;
	}
}

function isNewsukDir(cwd: string, rules: Rule[]): boolean {
	const resolved = resolve(cwd);
	for (const rule of rules) {
		if (rule.match === "*") return rule.profile === "newsuk";
		const expanded = resolve(expandHome(rule.match));
		if (resolved === expanded || resolved.startsWith(expanded + sep)) {
			return rule.profile === "newsuk";
		}
	}
	return false;
}

// ── Scoped Model Patterns ───────────────────────────────────────────────────

function buildPatterns(isNewsuk: boolean, ctx: ExtensionContext): string[] {
	if (isNewsuk) {
		// Only copilot + local models in scoped
		const patterns = [`${WORK_PROVIDER}/*`];
		for (const provider of LOCAL_PROVIDERS) {
			patterns.push(`${provider}/*`);
		}
		return patterns;
	}

	// Outside newsuk: all *available* models EXCEPT github-copilot.
	// Using getAvailable() (not getAll()) is deliberate: getAll() returns every
	// model pi knows about across ~48 providers, most of which have no
	// configured auth in this install. Scoping to getAll() made enabledModels
	// enumerate ~48 provider globs every session start, most resolving to zero
	// models — which is both slow (full registry walk + settings.json rewrite
	// on every startup) and noisy ("Warning: No models match pattern X/*" for
	// every unauthenticated provider). getAvailable() only returns models with
	// real configured auth (or local providers that don't need auth), so the
	// scoped list only ever contains providers that can actually resolve.
	const allModels = ctx.modelRegistry.getAvailable();
	const providers = new Set<string>();
	for (const m of allModels) {
		providers.add(m.provider);
	}
	providers.delete(WORK_PROVIDER);
	return Array.from(providers).sort().map((p) => `${p}/*`);
}

// ── Settings Persistence ─────────────────────────────────────────────────────

function writeEnabledModels(patterns: string[] | undefined): void {
	const agentDir = getAgentDir();
	const settingsPath = join(agentDir, "settings.json");

	let settings: Record<string, unknown> = {};
	try {
		settings = JSON.parse(readFileSync(settingsPath, "utf-8"));
	} catch {
		// File doesn't exist or is invalid — start fresh
	}

	if (patterns === undefined) {
		delete settings.enabledModels;
	} else {
		settings.enabledModels = patterns;
	}

	writeFileSync(settingsPath, JSON.stringify(settings, null, 2) + "\n", "utf-8");
}

// ── Status Tag ──────────────────────────────────────────────────────────────

function scopeTag(isNewsuk: boolean, theme: ExtensionContext["ui"]["theme"]): string {
	if (isNewsuk) {
		return theme.bold(theme.fg("error", "[newsuk]"));
	}
	return theme.bold(theme.fg("accent", "[scoped]"));
}

// ── Extension ───────────────────────────────────────────────────────────────

export default function scopedModelsExtension(pi: ExtensionAPI) {
	const rules = loadRules();

	pi.on("session_start", async (_event, ctx) => {
		const inNewsuk = isNewsukDir(ctx.cwd, rules);

		// Write the scoped-model patterns for future sessions
		const patterns = buildPatterns(inNewsuk, ctx);
		writeEnabledModels(patterns);

		// Show status indicator
		ctx.ui.setStatus("scoped", scopeTag(inNewsuk, ctx.ui.theme));

		// Notify on first run within this session
		const modelCount = ctx.modelRegistry.getAvailable().length;
		const visibleCount = inNewsuk
			? patterns.length * 10 // rough estimate
			: modelCount - (ctx.modelRegistry.getAvailable().filter((m) => m.provider === WORK_PROVIDER).length ?? 0);

		ctx.ui.notify(
			`scoped: ${inNewsuk ? "newsuk mode" : "other mode"} — ` +
				`${patterns.length} provider(s) in scope (~${Math.min(visibleCount, modelCount)} models). ` +
				`Use /scoped-models to customize, /reload to apply changes immediately.`,
			"info",
		);
	});

	pi.registerCommand("scoped", {
		description: "Show current scoped-model mode. See ~/.pi/agent/profiles.json for rules.",
		handler: async (_args, ctx) => {
			const inNewsuk = isNewsukDir(ctx.cwd, rules);
			const agentDir = getAgentDir();
			const isNewsukLabel = inNewsuk ? "YES" : "no";
			ctx.ui.notify(
				[
					`CWD in newsuk dir: ${isNewsukLabel}`,
					`  cwd  : ${ctx.cwd}`,
					`  rules: ${agentDir}/profiles.json`,
					`Scoped models: ${inNewsuk ? "copilot + local" : "all except copilot"}`,
					`Current model : ${ctx.model ? `${ctx.model.provider}/${ctx.model.id}` : "(none)"}`,
				].join("\n"),
				"info",
			);
		},
	});
}
