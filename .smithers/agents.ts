// smithers-source: generated
import { type AgentLike } from "smithers-orchestrator";
import { PiAgent as SmithersPiAgent } from "smithers-orchestrator";
import { VibeAgent as SmithersVibeAgent } from "smithers-orchestrator";
// import { OpenAIAgent as SmithersOpenAIAgent } from "smithers-orchestrator";
// import { KimiAgent as SmithersKimiAgent } from "smithers-orchestrator";
// import { AmpAgent as SmithersAmpAgent } from "smithers-orchestrator";
// import { HermesCliAgent as SmithersHermesCliAgent } from "smithers-orchestrator";
// import { OpenClawAgent as SmithersOpenClawAgent } from "smithers-orchestrator";
// import { ClaudeCodeAgent as SmithersClaudeCodeAgent } from "smithers-orchestrator";
// import { ClaudeCodeAgent } from "./agents/claude-code";
// import { CodexAgent } from "./agents/codex";
// import { OpenCodeAgent } from "./agents/opencode";
// import { AntigravityAgent } from "./agents/antigravity";

// export { ClaudeCodeAgent } from "./agents/claude-code";
// export { CodexAgent } from "./agents/codex";
// export { OpenCodeAgent } from "./agents/opencode";
// export { AntigravityAgent } from "./agents/antigravity";

export const providers = {
  lean: new SmithersVibeAgent({ agent: "lean", cwd: process.cwd() }),
  deepseekFlash: new SmithersPiAgent({ provider: "opencode-go", model: "deepseek-v4-flash" }),
  deepseekPro: new SmithersPiAgent({ provider: "opencode-go", model: "deepseek-v4-pro" }),
//   codex: CodexAgent,
//   opencode: OpenCodeAgent,
//   antigravity: AntigravityAgent,
//   claude: ClaudeCodeAgent,
//   kimi: new SmithersKimiAgent({ model: "kimi-k2.6" }),
//   amp: new SmithersAmpAgent(),
//   vibe: new SmithersVibeAgent({ agent: "auto-approve", cwd: process.cwd() }),
//   hermes: new SmithersHermesCliAgent({ cwd: process.cwd() }),
//   openclaw: new SmithersOpenClawAgent({ cwd: process.cwd() }),
//   claudeOpus: new SmithersClaudeCodeAgent({ model: "claude-opus-4-8", cwd: process.cwd() }),
//   claudeSonnet: new SmithersClaudeCodeAgent({ model: "claude-sonnet-5", cwd: process.cwd() }),
} as const;

export const agents = {
  cheapFast: [
    providers.deepseekFlash,
  ],
  smart: [
    providers.deepseekPro,
    providers.lean,
  ],
  smartTool: [
    providers.deepseekPro
  ],
  review: [
    providers.deepseekPro,
    providers.lean,
  ],
  planning: [
    providers.deepseekPro,
    providers.lean,
  ],
  implement: [
    providers.lean,
    providers.deepseekPro,
  ],
} as const satisfies Record<string, AgentLike[]>;
