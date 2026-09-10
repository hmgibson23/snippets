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

import { existsSync, readFileSync, writeFileSync, renameSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve, sep } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { getAgentDir } from "@earendil-works/pi-coding-agent";

/**
 */
const IS_SUBAGENT_CHILD = process.env.PI_SUBAGENT_CHILD === "1";

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

/**
 * Reads settings.json, updates enabledModels only if it actually changed, and
 * writes atomically. Returns true if a write happened.
 *
 * Safety properties (this replaced a version that caused real data loss):
 *   - Never resets settings to `{}` on a read/parse failure. If the file can't
 *     be read or parsed, we log a warning and bail out WITHOUT writing —
 *     leaving the existing file untouched is always safer than overwriting it
 *     with a guess. A transient read failure (e.g. another pi process mid-write
 *     to the same shared/symlinked settings.json) must never turn into data loss.
 *   - Atomic write: write to a temp file in the same directory, then rename()
 *     over the target. rename() is atomic on the same filesystem, so a
 *     concurrent reader (another pi process's session_start) either sees the
 *     old complete file or the new complete file — never a truncated/partial
 *     write. The previous writeFileSync() wrote directly to the live path,
 *     which could hand a concurrent reader invalid JSON mid-write.
 *   - Skips the write entirely when the computed patterns already match what's
 *     on disk — avoids a disk write (and the race window that comes with it)
 *     on every single session_start when nothing actually changed.
 */
function writeEnabledModelsIfChanged(patterns: string[] | undefined): boolean {
	const agentDir = getAgentDir();
	const settingsPath = join(agentDir, "settings.json");

	let settings: Record<string, unknown>;
	try {
		const raw = readFileSync(settingsPath, "utf-8");
		settings = raw.trim() ? JSON.parse(raw) : {};
	} catch (err) {
		if ((err as NodeJS.ErrnoException).code === "ENOENT") {
			settings = {};
		} else {
			// Read or parse failed for some other reason (e.g. concurrent writer,
			// transient I/O error). Do NOT reset to {} and do NOT write — that
			// would clobber every other key in settings.json with a guess.
			console.error(
				`[scoped-models] settings.json read/parse failed, skipping enabledModels update this run: ${err}`,
			);
			return false;
		}
	}

	const existing = Array.isArray(settings.enabledModels) ? (settings.enabledModels as unknown[]) : undefined;
	const unchanged =
		patterns === undefined
			? existing === undefined
			: existing !== undefined &&
				existing.length === patterns.length &&
				existing.every((v, i) => v === patterns[i]);
	if (unchanged) return false;

	if (patterns === undefined) {
		delete settings.enabledModels;
	} else {
		settings.enabledModels = patterns;
	}

	const tmpPath = `${settingsPath}.tmp-${process.pid}-${Date.now()}`;
	writeFileSync(tmpPath, JSON.stringify(settings, null, 2) + "\n", "utf-8");
	renameSync(tmpPath, settingsPath); // atomic on same filesystem
	return true;
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

		ctx.ui.setStatus("scoped", scopeTag(inNewsuk, ctx.ui.theme));

		// Write the scoped-model patterns for future sessions (only if changed)
		const patterns = buildPatterns(inNewsuk, ctx);
		const wrote = writeEnabledModelsIfChanged(patterns);

		// Only notify when something actually changed — this used to fire on
		// every single startup regardless, which was pure noise.
		if (!wrote) return;

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
