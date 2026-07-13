// smithers-source: seeded
// smithers-metadata-version: 1
// smithers-display-name: Implement coreutils batches
// smithers-description: For each batch: implement + proof + integrate + test, then review. Loop on failure.
// smithers-tags: implementation
/** @jsxImportSource smithers-orchestrator */
import { createSmithers, Sequence, Loop } from "smithers-orchestrator";
import { z } from "zod/v4";
import { agents } from "../agents";
import ImplementPrompt from "../prompts/implement.mdx";

// ── Schemas ──

const batchSchema = z.looseObject({
  title: z.string(),
  applets: z.array(z.string()),
  specialNote: z.string().default(""),
});

const planSchema = z.object({
  batches: z.array(batchSchema),
});

const implSchema = z.object({
  summary: z.string(),
  filesChanged: z.array(z.string()).default([]),
  allPassing: z.boolean().default(true),
});

const reviewSchema = z.object({
  approved: z.boolean(),
  feedback: z.string().default(""),
});

const { Workflow, Task, smithers, outputs } = createSmithers({
  input: planSchema,
  impl: implSchema,
  review: reviewSchema,
});

function buildPrompt(batch: z.input<typeof batchSchema>, feedback?: string): string {
  const names = batch.applets.join(", ");
  const base = `Implement these utilities: ${names}.

${batch.title}

${batch.specialNote ? `SPECIAL NOTES:\n${batch.specialNote}\n` : ""}

For each utility <name>:
1. Create Lentils/<name>/Logic.lean with pure functions + proofs
2. Create Lentils/<Name>/<Name>.lean with IO wrapper
3. Write a test for each <name>
4. Register in Lentils.lean and Main.lean

After all utilities:
5. Run: lake build
6. Fix any errors
7. Verify each: .lake/build/bin/lentils <name> --help
8. Run: .lake/build/bin/lentils --help  # verify listing

There is a reference for Lean itself and POSIX standards in /wiki. the Lean reference manual is written in .lean files.

Return the list of files changed.`;

  if (feedback) {
    return `${base}\n\n---\nPREVIOUS REVIEW FEEDBACK (fix these issues):\n${feedback}`;
  }
  return base;
}

function buildReviewPrompt(batch: z.input<typeof batchSchema>, impl?: z.input<typeof implSchema>): string {
  const base = `Review the implementation of: ${batch.applets.join(", ")}

Check:
1. Each utility has Lentils/<Name>/Logic.lean (pure functions) and Lentils/<name>/<name>.lean (IO wrapper)
2. Lentils.lean has imports for each
3. Main.lean has Applet entries
4. lake build succeeds
5. Each utility responds to --help
6. Each utility succesfully passed test and test covers all edge cases

Also check code quality:
- Pure functions are separated from IO
- Error messages go to stderr
- Return codes are correct (0 success, non-zero error)
- Follows existing patterns

If approved, return { approved: true }.
If not, return { approved: false, feedback: "what needs fixing" }.`;

  if (impl) {
    return `${base}\n\nFiles changed:\n${(impl.filesChanged ?? []).join("\n")}\n\nSummary: ${impl.summary}`;
  }
  return base;
}

export default smithers((ctx) => {
  const batches = ctx.input?.batches ?? [];
  if (batches.length === 0) {
    return (
      <Workflow name="implement-batches">
        <Task id="noop" output={outputs.impl}>
          {() => ({ summary: "No batches to process", allPassing: true })}
        </Task>
      </Workflow>
    );
  }

  // Check each batch's outputs from the last iteration
  const reviews = batches.map((_, i) =>
    ctx.outputMaybe("review", { nodeId: `batch-${i}:review` })
  );
  const impls = batches.map((_, i) =>
    ctx.outputMaybe("impl", { nodeId: `batch-${i}:impl` })
  );

  return (
    <Workflow name="implement-batches">
      <Sequence>
        {batches.map((batch, i) => {
          const lastReview = reviews[i];
          const lastImpl = impls[i];
          return (
            <Loop
              key={i.toString()}
              id={`batch-${i}`}
              until={lastReview?.approved === true}
              maxIterations={3}
              onMaxReached="return-last"
            >
              <Sequence>
                <Task
                  id={`batch-${i}:impl`}
                  output={outputs.impl}
                  agent={agents.smartTool}
                  timeoutMs={600_000}
                  heartbeatTimeoutMs={120_000}
                >
                  <ImplementPrompt prompt={buildPrompt(batch, lastReview?.feedback)} />
                </Task>
                <Task
                  id={`batch-${i}:review`}
                  output={outputs.review}
                  agent={agents.smartTool}
                >
                  <ImplementPrompt prompt={buildReviewPrompt(batch, lastImpl)} />
                </Task>
              </Sequence>
            </Loop>
          );
        })}
      </Sequence>
    </Workflow>
  );
});
