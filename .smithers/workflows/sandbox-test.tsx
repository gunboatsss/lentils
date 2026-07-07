// smithers-source: custom
// smithers-display-name: Sandbox Test
// smithers-description: Run differential sandbox tests in bwrap isolation with FS-state diffing. Self-correcting loop: build → test → fix → retest. No human approval — formal proofs + differential tests are the gate.
// smithers-tags: testing, verification, sandbox
/** @jsxImportSource smithers-orchestrator */
import { createSmithers } from "smithers-orchestrator";
import { z } from "zod/v4";
import { agents } from "../agents";
import { SandboxTest, NoSorryCheck, sandboxTestOutputSchema, noSorryOutputSchema } from "../components/SandboxTest";
import { implementOutputSchema } from "../components/ValidationLoop";
import ImplementPrompt from "../prompts/implement.mdx";

const outputSchema = z.looseObject({
  summary: z.string().default(""),
  allPassed: z.boolean().default(false),
  totalTests: z.number().default(0),
  passed: z.number().default(0),
  failed: z.number().default(0),
  noSorryPassed: z.boolean().default(false),
  buildSucceeded: z.boolean().default(false),
});

const inputSchema = z.object({
  prompt: z.string().default("Run all sandbox differential tests."),
  utility: z.string().default("all"),
  binaryPath: z.string().default(".lake/build/bin/coreutils"),
  maxIterations: z.number().int().min(1).max(10).default(3),
});

const { Workflow, Task, Sequence, Loop, smithers, outputs } = createSmithers({
  input: inputSchema,
  sandbox: sandboxTestOutputSchema,
  noSorry: noSorryOutputSchema,
  implement: implementOutputSchema,
  output: outputSchema,
});

const sandboxMemory = { kind: "workflow", id: "sandbox-test" } as const;

export default smithers((ctx) => {
  const sandboxResult = ctx.outputMaybe("sandbox", { nodeId: "sandbox:run" });
  const noSorryResult = ctx.outputMaybe("noSorry", { nodeId: "sandbox:nosorry" });

  // Determine if we're done (all gates pass)
  const testsPass = sandboxResult?.allPassed === true;
  const noSorryPass = noSorryResult?.passed === true;
  const done = testsPass && noSorryPass;

  // Build feedback for the implementation agent if anything failed
  const feedbackParts: string[] = [];
  if (sandboxResult && !testsPass) {
    feedbackParts.push(`SANDBOX TESTS FAILED (${sandboxResult.failed}/${sandboxResult.totalTests}):\n${sandboxResult.summary}`);
    for (const f of (sandboxResult.failures ?? []).slice(0, 10)) {
      feedbackParts.push(`  [${f.utility}] ${f.name}${f.fsMismatch ? " (FS STATE MISMATCH)" : ""}:\n${f.diff}`);
    }
  }
  if (noSorryResult && !noSorryPass) {
    feedbackParts.push(`NO-SORRY CHECK FAILED: found ${noSorryResult.violations?.length ?? 0} sorry/admit in Logic.lean files:\n${(noSorryResult.violations ?? []).map(v => `  ${v.file}:${v.line}: ${v.content}`).join("\n")}`);
  }
  const feedback = feedbackParts.length > 0 ? feedbackParts.join("\n\n") : null;

  return (
    <Workflow name="sandbox-test">
      <Sequence>
        <Loop id="sandbox:loop" until={done} maxIterations={ctx.input.maxIterations} onMaxReached="return-last">
          <Sequence>
            {/* Gate 1: implement/fix — agent writes code based on feedback */}
            <Task
              id="sandbox:implement"
              output={outputs.implement}
              agent={agents.implement}
              timeoutMs={1_800_000}
              heartbeatTimeoutMs={600_000}
              memory={{ remember: { namespace: sandboxMemory, key: "sandbox:implement" } }}
            >
              <ImplementPrompt prompt={feedback
                ? `${ctx.input.prompt}\n\n---\nVALIDATION FEEDBACK (fix these issues):\n${feedback}`
                : ctx.input.prompt} />
            </Task>

            {/* Gate 2: no-sorry audit — no cheating in proofs */}
            <NoSorryCheck
              id="sandbox:nosorry"
              agent={agents.cheapFast}
            />

            {/* Gate 3: differential sandbox tests — bwrap + FS-state diff */}
            <SandboxTest
              id="sandbox:run"
              utility={ctx.input.utility}
              binaryPath={ctx.input.binaryPath}
              agent={agents.cheapFast}
            />
          </Sequence>
        </Loop>

        {/* Final output */}
        <Task id="sandbox:output" output={outputs.output}>
          {() => ({
            summary: sandboxResult?.summary ?? "No tests ran",
            allPassed: done,
            totalTests: sandboxResult?.totalTests ?? 0,
            passed: sandboxResult?.passed ?? 0,
            failed: sandboxResult?.failed ?? 0,
            noSorryPassed: noSorryPass,
            buildSucceeded: true,
          })}
        </Task>
      </Sequence>
    </Workflow>
  );
});
