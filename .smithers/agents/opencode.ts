import { OpenCodeAgent as SmithersOpenCodeAgent } from "smithers-orchestrator";

// DeepseekFlash — used for all roles
const deepseekFlash = new SmithersOpenCodeAgent({
  model: "opencode-go/deepseek-v4-flash",
  cwd: process.cwd(),
});

export const OpenCodeAgent = deepseekFlash;
