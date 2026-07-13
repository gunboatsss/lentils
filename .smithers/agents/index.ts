import { OpenCodeAgent } from "./opencode";

const flash = OpenCodeAgent;

export const agents = {
  cheapFast: flash,
  smartTool: flash,
  smart: flash,
  review: flash,
  pm: flash,
  fullstack: flash,
  implementer: flash,
  reviewer: flash,
  tester: flash,
};

export { OpenCodeAgent };
export { ClaudeCodeAgent } from "./claude-code";
export { CodexAgent } from "./codex";
export { AntigravityAgent } from "./antigravity";
