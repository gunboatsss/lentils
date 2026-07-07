// smithers-source: custom
/** @jsxImportSource smithers-orchestrator */
import { Task, Sequence, Loop, type AgentLike } from "smithers-orchestrator";
import { z } from "zod/v4";
import SandboxTestPrompt from "../prompts/sandbox-test.mdx";
import { implementOutputSchema } from "./ValidationLoop";
import ImplementPrompt from "../prompts/implement.mdx";
import { implementer, panelists } from "./roles";

// ─── Schemas ───────────────────────────────────────────────────────────────────

const testFailureSchema = z.object({
  utility: z.string(),
  name: z.string(),
  diff: z.string(),
  fsMismatch: z.boolean().default(false),
});

export const sandboxTestOutputSchema = z.looseObject({
  summary: z.string(),
  allPassed: z.boolean().default(true),
  totalTests: z.number().default(0),
  passed: z.number().default(0),
  failed: z.number().default(0),
  failures: z.array(testFailureSchema).default([]),
});

export const noSorryOutputSchema = z.looseObject({
  passed: z.boolean().default(true),
  violations: z.array(z.object({
    file: z.string(),
    line: z.string(),
    content: z.string(),
  })).default([]),
});

// ─── Component: single sandbox test run ────────────────────────────────────────

type SandboxTestProps = {
  id: string;
  utility?: string;
  binaryPath?: string;
  agent: AgentLike | AgentLike[];
};

/**
 * <SandboxTest> — runs the differential sandbox test harness and reports
 * structured pass/fail results. The agent's job is purely to run the script
 * and parse its JSON output (deterministic, no creative work needed).
 *
 * The harness (tests/sandbox/run-sandbox-tests.sh) does:
 *   1. Run host coreutils in a bwrap sandbox → capture stdout/exit/FS-state
 *   2. Run our binary in a bwrap sandbox → same captures
 *   3. Diff stdout, exit code, and filesystem state
 *   4. Output JSON
 */
export function SandboxTest({ id, utility, binaryPath, agent }: SandboxTestProps) {
  return (
    <Task
      id={id}
      output={sandboxTestOutputSchema}
      agent={agent}
      timeoutMs={600_000}
      heartbeatTimeoutMs={300_000}
    >
      <SandboxTestPrompt
        utility={utility ?? "all"}
        binaryPath={binaryPath ?? ".lake/build/bin/coreutils"}
      />
    </Task>
  );
}

// ─── Component: no-sorry audit ─────────────────────────────────────────────────

type NoSorryProps = {
  id: string;
  agent: AgentLike | AgentLike[];
};

/**
 * <NoSorryCheck> — scans all Logic.lean files for `sorry` or `admit`,
 * which would bypass formal proofs. Must pass for verification to be real.
 */
export function NoSorryCheck({ id, agent }: NoSorryProps) {
  return (
    <Task
      id={id}
      output={noSorryOutputSchema}
      agent={agent}
      timeoutMs={120_000}
    >
      {`Run this command and report the results:

grep -rn 'sorry\\|admit' LeanCoreutils/**/Logic.lean

If there are NO matches, report { passed: true, violations: [] }.
If there ARE matches, report { passed: false, violations: [...] } with each match.

REQUIRED OUTPUT:
{props.schema}`}
    </Task>
  );
}

// ─── Component: full verification gate ─────────────────────────────────────────

type VerificationGateProps = {
  idPrefix: string;
  prompt: string;
  utility?: string;
  binaryPath?: string;
  agents: { implement: AgentLike[]; validate: AgentLike[]; review: AgentLike[] };
  maxIterations?: number;
};

/**
 * <VerificationGate> — the complete automated validation pipeline that replaces
 * human approval. Loops until ALL gates pass:
 *
 *   1. lake build           — proofs compile (failed proof = compile error)
 *   2. no-sorry check        — no cheating in Logic.lean
 *   3. sandbox differential  — stdout + stderr + exit + FS-state matches host
 *
 * If any gate fails, feeds the failures back to the implementation agent and
 * retries (up to maxIterations). No human in the loop.
 */
export function VerificationGate({
  idPrefix,
  prompt,
  utility,
  binaryPath,
  agents,
  maxIterations = 3,
}: VerificationGateProps) {
  // We can't read outputs before rendering, so the workflow function handles
  // the loop logic. This component just composes the inner sequence.
  // The calling workflow checks sandboxTestOutputSchema.allPassed and
  // noSorryOutputSchema.passed to decide done=.
  return null; // composed by the workflow's render function
}

export { implementer, panelists };
