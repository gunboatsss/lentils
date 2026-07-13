// smithers-source: seeded
// smithers-metadata-version: 1
// smithers-display-name: Strengthen proofs across all utilities
// smithers-description: For each utility, add non-trivial correctness theorems beyond basic native_decide examples.
// smithers-tags: proofs, verification, theorems
/** @jsxImportSource smithers-orchestrator */
import { createSmithers, Sequence } from "smithers-orchestrator";
import { z } from "zod/v4";
import { agents } from "../agents";
import ImplementPrompt from "../prompts/implement.mdx";

const groupSchema = z.object({
  title: z.string(),
  note: z.string().default(""),
  utilities: z.array(z.string()).default([]),
});

const planSchema = z.object({
  goal: z.string().default("Strengthen proofs across utilities"),
  groups: z.array(groupSchema).default([]),
});

const proofSchema = z.object({
  summary: z.string(),
  theoremsAdded: z.array(z.string()).default([]),
  filesChanged: z.array(z.string()).default([]),
  buildPassed: z.boolean().default(false),
});

const reviewSchema = z.object({
  approved: z.boolean(),
  feedback: z.string().default(""),
});

const { Workflow, Task, Loop, smithers, outputs } = createSmithers({
  input: planSchema,
  proofOutput: proofSchema,
  review: reviewSchema,
});

function buildProofPrompt(group: z.input<typeof groupSchema>): string {
  const names = group.utilities.join(", ");
  return `Strengthen proofs for: ${names}

Group: ${group.title}
${group.note}

For each utility:
1. Read Lentils/<name>/Logic.lean and understand the pure functions.
2. Add meaningful theorems:
   - Identity/idempotence laws (no-op yields same input)
   - Roundtrip properties (encode then decode = identity)
   - Invariant preservation (output property = input property)
   - Edge cases (empty input, max values, special chars)
   - Algebraic properties (associativity, commutativity)
   - Structural induction for recursive functions
3. Use appropriate proof techniques:
   - native_decide for concrete decidable examples
   - rfl / simp for simple equalities
   - induction for recursive functions
   - quantifier statements where possible
4. Run: lake build
5. Fix any proof errors.

Return summary of theorems added per utility.`;
}

function buildReviewPrompt(group: z.input<typeof groupSchema>): string {
  return `Review proofs added for: ${group.utilities.join(", ")}

Check:
1. Each theorem has a meaningful doc comment
2. Proofs use appropriate techniques (native_decide, rfl, simp, induction)
3. No sorry or admit
4. lake build succeeds
5. Theorems test non-trivial properties

If approved, return { approved: true }.
If not, return { approved: false, feedback: "what's missing" }.`;
}

export default smithers((ctx) => {
  const groups = ctx.input?.groups ?? [];
  if (groups.length === 0) {
    return (
      <Workflow name="strengthen-proofs">
        <Task id="noop" output={outputs.proofOutput}>
          {() => ({ summary: "No groups specified", buildPassed: true })}
        </Task>
      </Workflow>
    );
  }

  const reviews = groups.map((_, i) =>
    ctx.outputMaybe("review", { nodeId: `group-${i}:review` })
  );

  return (
    <Workflow name="strengthen-proofs">
      <Sequence>
        {groups.map((group, i) => (
          <Loop
            key={group.title}
            id={`group-${i}`}
            until={reviews[i]?.approved === true}
            maxIterations={3}
            onMaxReached="return-last"
          >
            <Sequence>
              <Task
                id={`group-${i}:proof`}
                output={outputs.proofOutput}
                agent={agents.smartTool}
                timeoutMs={300_000}
                heartbeatTimeoutMs={120_000}
              >
                <ImplementPrompt prompt={buildProofPrompt(group)} />
              </Task>
              <Task
                id={`group-${i}:review`}
                output={outputs.review}
                agent={agents.smartTool}
              >
                <ImplementPrompt prompt={buildReviewPrompt(group)} />
              </Task>
            </Sequence>
          </Loop>
        ))}
      </Sequence>
    </Workflow>
  );
});
