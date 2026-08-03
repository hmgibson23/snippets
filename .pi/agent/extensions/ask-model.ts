import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "typebox";
import { Container, DynamicBorder, Key, matchesKey, SelectList, Text } from "@mariozechner/pi-tui";

export default function askModelExtension(pi: ExtensionAPI) {
	pi.registerTool({
		name: "ask_model",
		label: "Ask Model",
		description: "Ask the user which model to use for the next step (e.g. implementation). Blocks until the user selects a model. Returns the selected model's ID.",
		parameters: Type.Object({
			reason: Type.String({ description: "Why you are asking for a model choice (e.g., 'to write the code for the auth flow')" })
		}),

		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			if (!ctx.hasUI) {
				return {
					content: [{ type: "text", text: "Error: Cannot ask for model in non-interactive mode. Please proceed with the current model." }],
					details: {}
				};
			}

			// Get all available models
			const models = ctx.modelRegistry.getAll().map(m => ({
				value: `${m.provider}/${m.id}`,
				label: `${m.provider}/${m.id}`,
				description: m.name !== m.id ? m.name : undefined
			}));

			// Add a default fallback
			models.unshift({
				value: "current",
				label: "Keep using current model",
				description: "Proceed with the model currently selected in Pi"
			});

			const choice = await ctx.ui.custom<string | null>((tui, theme, _kb, done) => {
				const container = new Container();
				container.addChild(new DynamicBorder((str) => theme.fg("accent", str)));

				// Header
				container.addChild(new Text(theme.fg("accent", theme.bold(`Which model should I use ${params.reason}?`))));

				// SelectList with typed search filtering built in
				const selectList = new SelectList(models, Math.min(models.length, 5), {
					selectedPrefix: (text) => theme.fg("accent", text),
					selectedText: (text) => theme.fg("accent", text),
					description: (text) => theme.fg("muted", text),
					scrollInfo: (text) => theme.fg("dim", text),
					noMatch: (text) => theme.fg("warning", text),
				});

				selectList.onSelect = (item) => done(item.value);
				selectList.onCancel = () => done(null);

				container.addChild(selectList);

				// Footer hint
				container.addChild(new Text(theme.fg("dim", "↑↓ navigate • type to search • enter select • esc cancel")));

				container.addChild(new DynamicBorder((str) => theme.fg("accent", str)));

				return {
					render(width: number) {
						return container.render(width);
					},
					invalidate() {
						container.invalidate();
					},
					handleInput(data: string) {
						selectList.handleInput(data);
						tui.requestRender();
					},
				};
			});

			if (!choice) {
				return {
					content: [{ type: "text", text: "User cancelled the selection. Please proceed with the current model." }],
					details: {}
				};
			}

			if (choice === "current") {
				return {
					content: [{ type: "text", text: "User chose to keep using the current model." }],
					details: { choice: "current" }
				};
			}

			return {
				content: [{ type: "text", text: `User selected model: ${choice}` }],
				details: { model: choice }
			};
		}
	});
}
