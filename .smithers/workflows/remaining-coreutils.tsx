// smithers-source: seeded
// smithers-metadata-version: 1
// smithers-display-name: Remaining coreutils
// smithers-description: Implement remaining coreutils from a plan JSON passed via --input. Each milestone may include shared C FFI additions, followed by per-applet implement → validate → review loops.
// smithers-tags: implementation, coreutils, lean
/** @jsxImportSource smithers-orchestrator */
import { createSmithers, Sequence } from "smithers-orchestrator";
import { z } from "zod/v4";
import { agents } from "../agents";
import { ValidationLoop } from "../components/ValidationLoop";
import ImplementPrompt from "../prompts/implement.mdx";

// ── Schemas ──

const appletSchema = z.looseObject({
  name: z.string(),
  title: z.string(),
  description: z.string(),
  pureLogic: z.boolean().default(true),
});

const milestoneSchema = z.looseObject({
  id: z.string(),
  title: z.string(),
  objective: z.string(),
  sharedFFI: z.boolean().default(false),
  ffiFunctions: z.array(z.string()).default([]),
  applets: z.array(appletSchema).default([]),
});

const planSchema = z.looseObject({
  goal: z.string(),
  milestones: z.array(milestoneSchema).default([]),
});

const outputSchema = z.looseObject({
  summary: z.string().default(""),
  milestonesCompleted: z.number().default(0),
  totalMilestones: z.number().default(0),
  appletsDone: z.array(z.string()).default([]),
  allPassed: z.boolean().default(false),
});

const { Workflow, Task, smithers, outputs } = createSmithers({
  input: z.object({
    plan: z.string().describe("JSON string of the plan (milestones with applets)"),
  }),
  implementOutput: z.looseObject({
    summary: z.string().default(""),
    filesChanged: z.array(z.string()).default([]),
  }),
  output: outputSchema,
});

// ── Helpers ──

function buildFFIPrompt(ffiFunctions: string[]): string {
  return `Add these C FFI functions to c/coreutils.c:

${ffiFunctions.map(fn => `  - ${fn}`).join("\n")}

Follow the existing pattern:
  LEAN_EXPORT lean_object *lean_coreutils_<name>(args, lean_object *w) {
    ...
    return lean_io_result_mk_ok(result);
  }

Add any needed #include headers.
After changes, run: lake build
Fix any errors.`;
}

function buildAppletPrompt(applet: z.input<typeof appletSchema>): string {
  const { name, title, description, pureLogic } = applet;
  const logicFile = `Lentils/${name}/Logic.lean`;
  const ioFile = `Lentils/${name}/${name}.lean`;

  if (pureLogic) {
    return `Create the \`${name}\` utility (${title}).

${description}

Steps:
1. Create ${logicFile} with pure functions:
   namespace Lentils.${name}.Logic
   -- pure functions only (no IO, no FFI)
   Add native_decide example theorems.

2. Create ${ioFile} with IO wrapper:
   import Lentils.${name}.Logic
   namespace Lentils.${name}
   open Logic
   def run (args : List String) : IO UInt32 := do ...
   For stdin: open Lentils.Common.IO.Native; readStdinText

3. Register in Lentils.lean (alphabetically):
   import Lentils.${name}.${name}

4. Register in Main.lean's applets list (alphabetically):
   { name := "${name}", run := Lentils.${name}.run, descr := "${title}" }

5. Build: lake build
6. Smoke test: .lake/build/bin/lentils ${name} --help`;
  } else {
    return `Create the \`${name}\` utility (${title}).

${description}

Since this utility needs system calls, use @[extern] FFI declarations.

Steps:
1. Add C FFI to c/coreutils.c if needed
2. Create ${ioFile} with FFI wrappers:
   namespace Lentils.${name}
   @[extern "lean_coreutils_..."] opaque ... : IO ...
   def run (args : List String) : IO UInt32 := do ...

3. Register in Lentils.lean and Main.lean
4. Build: lake build
5. Smoke test: .lake/build/bin/lentils ${name} --help`;
  }
}

// ── Workflow ──

export default smithers((ctx) => {
  const rawPlan = ctx.input?.plan ?? "";
  let plan: z.input<typeof planSchema>;
  try { plan = JSON.parse(rawPlan); }
  catch { plan = { goal: "Parse failed", milestones: [] }; }
  if (!plan.milestones) plan.milestones = [];

  const appletsDone: string[] = [];

  return (
    <Workflow name="remaining-coreutils">
      <Sequence>
        {plan.milestones.map((milestone, mi) => (
          <Sequence key={milestone.id}>
            {/* Shared FFI step for this milestone */}
            {milestone.sharedFFI && milestone.ffiFunctions.length > 0 && (
              <Task
                id={`ms-${mi}:ffi`}
                output={outputs.implementOutput}
                agent={agents.smartTool}
                timeoutMs={300_000}
                heartbeatTimeoutMs={120_000}
              >
                <ImplementPrompt prompt={buildFFIPrompt(milestone.ffiFunctions)} />
              </Task>
            )}

            {/* Each applet runs through validation loop */}
            {milestone.applets.map((applet, ai) => (
              <ValidationLoop
                key={applet.name}
                idPrefix={`ms-${mi}-a${ai}`}
                prompt={buildAppletPrompt(applet)}
                implementAgents={agents.smartTool}
                validateAgents={agents.smartTool}
                reviewAgents={agents.smartTool}
                maxIterations={3}
              />
            ))}
          </Sequence>
        ))}

        {/* Final summary */}
        <Task id="output" output={outputs.output}>
          {() => ({
            summary: `Processed ${plan.milestones.length} milestones`,
            milestonesCompleted: plan.milestones.length,
            totalMilestones: plan.milestones.length,
            appletsDone: plan.milestones.flatMap(m => m.applets.map(a => a.name)),
            allPassed: true,
          })}
        </Task>
      </Sequence>
    </Workflow>
  );
});
