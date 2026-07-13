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
  hy3Free: new SmithersPiAgent({ provider: "opencode", model: "hy3-free" }),
  deepseekFlashFree: new SmithersPiAgent({ provider: "opencode", model: "deepseek-v4-flash-free" }),
  mimoV25Free: new SmithersPiAgent({ provider: "opencode", model: "mimo-v2.5-free" }),
  nemotron3UltraFree: new SmithersPiAgent({ provider: "opencode", model: "nemotron-3-ultra-free" }),
  northMiniCodeFree: new SmithersPiAgent({ provider: "opencode", model: "north-mini-code-free" }),
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
  // Fast/cheap — quick turnarounds, simple tasks
  cheapFast: [
    providers.deepseekFlash
  ],
  // Smart reasoning — complex logic, proofs, architecture
  smart: [
    providers.deepseekPro,   // out of credits
    // providers.lean,          // out of credits
    providers.hy3Free,
    providers.nemotron3UltraFree,
  ],
  // Tool-heavy agentic tasks
  smartTool: [
    // providers.deepseekPro,   // out of credits
    providers.deepseekFlash,
  ],
  // Code review (nemotron: 1M ctx for full-project analysis; mimo: vision for diagrams)
  review: [
    // providers.deepseekPro,   // out of credits
    // providers.lean,          // out of credits
    providers.deepseekPro
  ],
  // Architecture & implementation planning
  planning: [
    // providers.deepseekPro,   // out of credits
    // providers.lean,          // out of credits
    providers.deepseekPro
  ],
  // Hands-on implementation & coding
  implement: [
    // providers.lean,          // out of credits
    // providers.deepseekPro,   // out of credits
    providers.deepseekFlash
  ],
} as const satisfies Record<string, AgentLike[]>;
