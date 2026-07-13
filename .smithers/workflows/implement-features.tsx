// smithers-source: seeded
// smithers-metadata-version: 1
// smithers-display-name: Implement Features
// smithers-description: Execute a hand-written implementation plan across multiple utilities, with validation loops between milestones.
// smithers-tags: implementation, multi-utility, plan-driven
/** @jsxImportSource smithers-orchestrator */
import { createSmithers } from "smithers-orchestrator";
import { z } from "zod/v4";
import { agents } from "../agents";
import { ValidationLoop, implementOutputSchema, validateOutputSchema } from "../components/ValidationLoop";
import { reviewOutputSchema, reviewSynthesisSchema } from "../components/Review";

// ── Plan schema ──

const featureSchema = z.looseObject({
  id: z.string(),
  title: z.string(),
  instructions: z.string(),
  files: z.array(z.string()).default([]),
  validation: z.array(z.string()).default([]),
});

const milestoneSchema = z.looseObject({
  id: z.string(),
  title: z.string(),
  objective: z.string(),
  features: z.array(featureSchema).default([]),
  prerequisites: z.array(z.string()).default([]),
});

const planSchema = z.looseObject({
  goal: z.string(),
  milestones: z.array(milestoneSchema).default([]),
  notes: z.array(z.string()).default([]),
});

// ── Run output ──

const outputSchema = z.looseObject({
  summary: z.string().default(""),
  milestonesCompleted: z.number().default(0),
  totalMilestones: z.number().default(0),
  filesChanged: z.array(z.string()).default([]),
  allPassed: z.boolean().default(false),
  remainingWork: z.array(z.string()).default([]),
});

const { Workflow, Task, Sequence, smithers, outputs } = createSmithers({
  input: z.object({
    plan: z.string().default(""),
    startAt: z.string().optional(),
  }),
  planOutput: planSchema,
  implement: implementOutputSchema,
  validate: validateOutputSchema,
  review: reviewOutputSchema,
  reviewSynthesis: reviewSynthesisSchema,
  output: outputSchema,
});

// ── Helpers ──

function parsePlan(raw: string): z.input<typeof planSchema> | null {
  if (!raw) return null;
  try {
    return JSON.parse(raw) as z.input<typeof planSchema>;
  } catch {
    return null;
  }
}

// ── Workflow ──

export default smithers((ctx) => {
  const rawPlan = ctx.input?.plan ?? "";
  const plan = ctx.outputMaybe("planOutput", { nodeId: "init:parse-plan" });
  const parsed = plan ?? parsePlan(rawPlan);

  const milestoneOutputs = ctx.outputs.milestoneOutput ?? [];
  const currentMilestoneIdx = milestoneOutputs.length;

  // Parse the plan if not already done
  if (!parsed) {
    return (
      <Workflow name="implement-features">
        <Task id="init:parse-plan" output={outputs.planOutput} agent={agents.smartTool}>
          {() => {
            const p = parsePlan(rawPlan);
            if (p) return { goal: p.goal, milestones: p.milestones, notes: p.notes };
            return {
              goal: "Parse failed",
              milestones: [],
              notes: [
                `Could not parse plan JSON from input.`,
                `Input: ${rawPlan ? rawPlan.slice(0, 200) + "..." : "empty (provide --input with a plan JSON)"}`,
              ],
            };
          }}
        </Task>
      </Workflow>
    );
  }

  const milestones = parsed.milestones;
  const remaining = milestones.slice(currentMilestoneIdx);
  const done = milestones.slice(0, currentMilestoneIdx);
  const allPassed = done.length > 0 && done.length === milestones.length;

  return (
    <Workflow name="implement-features">
      <Sequence>
        {remaining.map((milestone, idx) => {
          const globalIdx = currentMilestoneIdx + idx;
          const prefix = `ms-${globalIdx}`;

          return (
            <Sequence key={milestone.id}>
              <Task id={`${prefix}:header`} output={outputs.planOutput}>
                {() => ({
                  goal: parsed.goal,
                  milestones: parsed.milestones,
                  notes: [...parsed.notes, `▶ Running: ${milestone.title}`],
                })}
              </Task>

              {milestone.features.map((feature, fi) => {
                const fPrefix = `${prefix}-f${fi}`;
                // Build a prompt that includes instructions, files, and validation criteria
                const prompt = [
                  feature.instructions,
                  feature.files.length > 0 ? `\nFiles to modify:\n${feature.files.map(f => `  - ${f}`).join("\n")}` : "",
                  feature.validation.length > 0 ? `\nValidation criteria:\n${feature.validation.map(v => `  - ${v}`).join("\n")}` : "",
                ].join("\n").trim();

                return (
                  <ValidationLoop
                    key={feature.id}
                    idPrefix={fPrefix}
                    prompt={prompt}
                    implementAgents={agents.smartTool}
                    validateAgents={agents.cheapFast}
                    reviewAgents={agents.review}
                    synthesizeReview={false}
                    maxIterations={3}
                  />
                );
              })}

              <Task id={`${prefix}:done`} output={outputs.planOutput}>
                {() => ({
                  goal: parsed.goal,
                  milestones: milestones.map((m, mi) =>
                    mi <= globalIdx ? { ...m, status: "completed" } : m
                  ),
                  notes: [...parsed.notes, `✓ Completed: ${milestone.title}`],
                })}
              </Task>
            </Sequence>
          );
        })}

        <Task id="output" output={outputs.output}>
          {() => {
            const impls = ctx.outputs.implement ?? [];
            const allFiles = impls.flatMap(r => r.filesChanged ?? []);
            const uniqueFiles = [...new Set(allFiles)];
            return {
              summary: `Completed ${done.length}/${milestones.length} milestones`,
              milestonesCompleted: done.length,
              totalMilestones: milestones.length,
              filesChanged: uniqueFiles,
              allPassed,
              remainingWork: allPassed ? [] : remaining.map(m => `${m.title}: ${m.objective}`),
            };
          }}
        </Task>
      </Sequence>
    </Workflow>
  );
});
