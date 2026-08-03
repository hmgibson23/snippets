import path from "node:path";

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

function formatModel(ctx: ExtensionContext): string {
	if (!ctx.model) {
		return "unknown";
	}

	return `${ctx.model.provider}/${ctx.model.id}`;
}

function setTopbar(ctx: ExtensionContext): void {
	if (!ctx.hasUI) {
		return;
	}

	const model = formatModel(ctx);
	const cwdName = path.basename(ctx.cwd) || ctx.cwd;
	ctx.ui.setTitle(`π ${cwdName} • ${model}`);
	ctx.ui.setStatus("model", `🤖 ${model}`);
}

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		setTopbar(ctx);
	});

	pi.on("model_select", async (_event, ctx) => {
		setTopbar(ctx);
	});
}
