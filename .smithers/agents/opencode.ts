import { OpenCodeAgent as SmithersOpenCodeAgent } from "smithers-orchestrator";

// DeepseekFlash — used for all roles
const deepseekFlash = new SmithersOpenCodeAgent({
  model: "deepseek/deepseek-flash",
  cwd: process.cwd(),
});

export const OpenCodeAgent = deepseekFlash;
