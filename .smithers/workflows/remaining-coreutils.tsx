// smithers-source: seeded
// smithers-metadata-version: 1
// smithers-display-name: Remaining coreutils (~25 utilities)
// smithers-description: Implement all remaining coreutils across 5 milestones grouped by shared requirements (pure Lean IO, C FFI, fork/exec, text processing, date/time).
// smithers-tags: implementation, file-ops, process, text-processing, tier-3
/** @jsxImportSource smithers-orchestrator */
import { createSmithers } from "smithers-orchestrator";
import { z } from "zod/v4";
import { agents } from "../agents";
import { ValidationLoop, implementOutputSchema, validateOutputSchema } from "../components/ValidationLoop";

// ── Plan schema ──

const appletSchema = z.looseObject({
  name: z.string(),
  title: z.string(),
  description: z.string(),
  pureLogic: z.boolean().default(true),
  files: z.array(z.string()).default([]),
  tests: z.array(z.string()).default([]),
});

const milestoneSchema = z.looseObject({
  id: z.string(),
  title: z.string(),
  objective: z.string(),
  sharedFFI: z.boolean().default(false),
  sharedForkExec: z.boolean().default(false),
  ffiFunctions: z.array(z.string()).default([]),
  applets: z.array(appletSchema).default([]),
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
  milestones: z.array(z.looseObject({
    id: z.string(),
    status: z.enum(["done", "skipped", "failed"]),
    applets: z.array(z.string()).default([]),
  })).default([]),
  ffiChanges: z.string().default(""),
  newCount: z.number().default(0),
});

export default createSmithers({
  meta: {
    name: "remaining-coreutils",
    description: "Implement all remaining coreutils (~25 utilities) for lean-coreutils",
  },
  inputSchema: planSchema,
  outputSchema,
  agents: {
    planner: agents.pm,
    implementer: agents.fullstack,
    reviewer: agents.reviewer,
    tester: agents.tester,
  },
  async execute(plan, { logger, context }) {
    const results = [];
    let ffiChanges = "";

    for (const milestone of plan.milestones) {
      logger.info(`Milestone: ${milestone.title} — ${milestone.objective}`);

      // Step 0: Shared C FFI (if needed for this milestone)
      if (milestone.sharedFFI && milestone.ffiFunctions.length > 0) {
        logger.info(`  Adding C FFI: ${milestone.ffiFunctions.join(", ")}`);
        await context.call("implement", {
          files: ["c/coreutils.c"],
          instructions: `Add the following C FFI functions to c/coreutils.c:
${milestone.ffiFunctions.map(fn => `  - ${fn}`).join("\n")}
Follow the existing pattern: LEAN_EXPORT lean_object *lean_coreutils_<name>(args, lean_object *w).
Return lean_io_result_mk_ok(result) on success, lean_io_result_mk_error(...) on error.
Add #include headers as needed at the top of the file.`,
          agent: "implementer",
        });
        ffiChanges += `C FFI: ${milestone.ffiFunctions.join(", ")}\n`;
      }

      // Step 0b: Shared fork/exec infrastructure (if needed)
      if (milestone.sharedForkExec) {
        logger.info(`  Adding fork/exec infrastructure to c/coreutils.c`);
        await context.call("implement", {
          files: ["c/coreutils.c"],
          instructions: `Add fork/exec utility functions to c/coreutils.c:
- A helper to find an executable in PATH (find_in_path)
- A helper to build argv/envp arrays
- A lean_coreutils_run_cmd function that forks and execs (follow the env pattern)
Return exit code as boxed uint32 via lean_io_result_mk_ok.`,
          agent: "implementer",
        });
        ffiChanges += "C FFI: fork/exec infrastructure\n";
      }

      // Implement each applet in this milestone
      for (const applet of milestone.applets) {
        logger.info(`  Implementing: ${applet.name} — ${applet.title}`);

        // Step 1: Create Logic.lean (pure functions)
        // Only create if the utility has meaningful pure logic
        // (file-op utilities like cp, mv, rm have minimal pure logic)
        if (applet.pureLogic) {
          await context.call("implement", {
            files: [`Lentils/${applet.name}/Logic.lean`],
            instructions: `Create Lentils/${applet.name}/Logic.lean with pure functions for ${applet.title}.
${applet.description}
Follow the established pattern: namespace Lentils.${applet.name}.Logic, pure functions only (no IO).
Add native_decide example theorems at the bottom.
This file must contain ONLY pure Lean — no FFI, no IO.`,
            agent: "implementer",
          });
        }

        // Step 2: Create IO wrapper
        await context.call("implement", {
          files: [`Lentils/${applet.name}/${applet.name}.lean`],
          instructions: `Create Lentils/${applet.name}/${applet.name}.lean with the IO wrapper for ${applet.title}.
${applet.description}
Must follow the pattern:
  namespace Lentils.${applet.name}
  open Logic
  def run (args : List String) : IO UInt32 := do ...
Import Lentils.${applet.name}.Logic (or use FFI opaque declarations if no pure logic).
${applet.pureLogic ? "" : "This utility has minimal pure logic. Use @[extern] FFI declarations for system calls."}
For reading stdin, use: open Lentils.Common.IO.Native; readStdinText (for String) or readStdinLines (for List String).
Always support --help: use printHelp pattern (but that's handled by the Applet list in Main.lean).
Return 0 on success, non-zero on error with error message to stderr.`,
            agent: "implementer",
          });
        });

        // Step 3: Wire into Lentils.lean and Main.lean
        await context.call("implement", {
          files: ["Lentils.lean", "Main.lean"],
          instructions: `Register the ${applet.name} utility:
1. Add 'import Lentils.${applet.name}.${applet.name}' to Lentils.lean (alphabetically).
2. Add an Applet entry for ${applet.name} to the 'applets' list in Main.lean:
   { name := "${applet.name}", run := Lentils.${applet.name}.run, descr := "${applet.title}" },
Place it in alphabetical order among the existing entries.`,
          agent: "implementer",
        });

        // Step 4: Build
        await context.call("build", {
          command: "lake build 2>&1",
          agent: "tester",
        });

        // Step 5: Quick smoke test
        await context.call("test", {
          command: `.lake/build/bin/lentils ${applet.name} --help 2>&1`,
          agent: "tester",
        });

        results.push({ applet: applet.name, status: "done" });
      }
    }

    return {
      summary: `Implemented ${results.length} utilities across ${plan.milestones.length} milestones`,
      milestones: plan.milestones.map(m => ({
        id: m.id,
        status: "done",
        applets: m.applets.map(a => a.name),
      })),
      ffiChanges,
      newCount: results.length,
    };
  },
});
