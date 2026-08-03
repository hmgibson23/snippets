import { readFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function localModelsManagerExtension(pi: ExtensionAPI) {
  console.log("🧠 Local models manager loaded");
  
  // Initialize provider
  initializeLocalModelsProvider(pi);
  
  // Show commands
  pi.registerCommand("local-models", {
    description: "Manage local LLM endpoints",
    handler: async (args, ctx) => {
      const [action, ...params] = args.split(" ");
      
      switch (action) {
        case "list":
          await listEndpoints(ctx);
          break;
        case "add":
          if (params.length < 2) {
            ctx.ui.notify("Usage: /local-models add <name> <baseUrl>", "error");
            return;
          }
          await addEndpoint(ctx, params[0], params[1]);
          break;
        default:
          showLocalModelsHelp(ctx);
          break;
      }
    },
  });
}

function initializeLocalModelsProvider(pi: ExtensionAPI) {
  const localModels = loadLocalModelsConfig();
  
  pi.registerProvider("local-models", {
    baseUrl: "http://localhost:8080/v1",
    apiKey: "$LOCAL_MODELS_API_KEY",
    api: "openai-completions",
    models: localModels.map(model => ({
      id: model.id,
      name: model.name,
      reasoning: false,
      input: ["text"],
      cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
      contextWindow: model.contextWindow || 128000,
      maxTokens: model.maxTokens || 4096,
    }))
  });
}

async function listEndpoints(ctx: any) {
  const config = loadLocalModelsConfig();
  let output = "=== Local Models Manager ===\n\n";
  
  if (config.length === 0) {
    output += "No local endpoints configured.\n";
    output += "Use: /local-models add <name> <baseUrl> [model1 model2...]\n";
  } else {
    output += "Available Models:\n";
    config.forEach(model => {
      output += `- ${model.name} (${model.id})\n`;
      output += `  Context: ${model.contextWindow || "unknown"} tokens\n`;
      output += `  Max tokens: ${model.maxTokens || "unknown"}\n\n`;
    });
  }
  
  ctx.ui.notify(output, "info");
}

async function addEndpoint(ctx: any, name: string, baseUrl: string) {
  ctx.ui.notify("Add endpoint functionality to be implemented: " + name + " at " + baseUrl, "info");
}

function showLocalModelsHelp(ctx: any) {
  const help = "=== Local Models Manager Help ===\n\n" +
    "Commands:\n" +
    "  /local-models list          - List configured endpoints\n" +
    "  /local-models add <name> <baseUrl> - Add new endpoint\n" +
    "  /local-models help          - Show this help\n\n" +
    "Example:\n" +
    "  /local-models add ollama http://localhost:11434/v1 llama3.2 mixtral\n";
  
  ctx.ui.notify(help, "info");
}

function loadLocalModelsConfig(): any[] {
  try {
    const configFile = join(process.env.HOME || ".", ".pi", "agent", "local-models.json");
    
    if (!existsSync(configFile)) {
      return [
        { id: "llama3.2", name: "Llama 3.2", contextWindow: 8192, maxTokens: 4096 },
        { id: "mixtral", name: "Mixtral", contextWindow: 32768, maxTokens: 8192 },
      ];
    }
    
    const data = readFileSync(configFile, 'utf8');
    const config = JSON.parse(data);
    return config.endpoints.flatMap((endpoint: any) => 
      endpoint.models.map((model: any) => ({ ...model, endpoint: endpoint.id }))
    );
  } catch (error) {
    console.error("Failed to load local models config:", error);
    return [];
  }
}