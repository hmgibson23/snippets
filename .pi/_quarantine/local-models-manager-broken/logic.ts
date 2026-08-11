// Local models manager logic
import { readFileSync, writeFileSync, existsSync } from "fs";
import { join } from "path";

interface LocalModel {
  id: string;
  name: string;
  contextWindow?: number;
  maxTokens?: number;
}

interface LocalModelsConfig {
  endpoints: {
    id: string;
    name: string;
    baseUrl: string;
    apiKey?: string;
    models: LocalModel[];
  }[];
}

const CONFIG_FILE = join(process.env.HOME || ".", ".pi", "agent", "local-models.json");

export function loadLocalModelsConfig(): LocalModel[] {
  if (!existsSync(CONFIG_FILE)) {
    return [
      { id: "llama3.2", name: "Llama 3.2", contextWindow: 8192, maxTokens: 4096 },
      { id: "mixtral", name: "Mixtral", contextWindow: 32768, maxTokens: 8192 },
    ];
  }
  
  try {
    const data = readFileSync(CONFIG_FILE, "utf8");
    const config = JSON.parse(data);
    return config.endpoints.flatMap(endpoint => 
      endpoint.models.map(model => ({ ...model, endpoint: endpoint.id }))
    );
  } catch (error) {
    console.error("Failed to load local models config:", error);
    return [];
  }
}