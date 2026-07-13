// smithers-source: seeded
// smithers-metadata-version: 1
// smithers-display-name: Tier 1 — System Info & Trivial Utilities
// smithers-description: Implement 13 trivial utilities (uname, arch, hostid, logname, tty, whoami, uptime, printenv, nproc, users, groups, id, seq) that share the syscall→format→print pattern.
// smithers-tags: implementation, system-info, tier-1
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
  requiresFFI: z.boolean().default(false),
  files: z.array(z.string()).default([]),
  tests: z.array(z.string()).default([]),
});

const milestoneSchema = z.looseObject({
  id: z.string(),
  title: z.string(),
  objective: z.string(),
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
  newCount: z.number().default(0),
});

export default createSmithers({
  meta: {
    name: "tier1-system-info",
    description: "Implement 13 Tier-1 (trivial) utilities for lean-coreutils",
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

    for (const milestone of plan.milestones) {
      logger.info(`Milestone: ${milestone.title}`);

      for (const applet of milestone.applets) {
        logger.info(`  Implementing: ${applet.name}`);

        // Step 1: Create Logic.lean (pure functions)
        await context.call("implement", {
          files: applet.files,
          instructions: `Create Lentils/${applet.name}/Logic.lean with the pure logic for ${applet.title}. ${applet.description}`,
          agent: "implementer",
        });

        // Step 2: Create IO wrapper
        await context.call("implement", {
          files: [`Lentils/${applet.name}/${applet.name}.lean`],
          instructions: `Create Lentils/${applet.name}/${applet.name}.lean with the IO wrapper for ${applet.title}. Must export: def run (args : List String) : IO UInt32`,
          agent: "implementer",
        });

        // Step 3: Create re-export module
        await context.call("write-file", {
          path: `Lentils/${applet.name}.lean`,
          content: `/- Lentils.${applet.name} — Re-export module for the \`${applet.name}\` utility. -/\nimport Lentils.${applet.name}.Logic\nimport Lentils.${applet.name}.${applet.name}\n`,
        });

        // Step 4: Wire into Lentils.lean and Main.lean
        await context.call("implement", {
          files: ["Lentils.lean", "Main.lean"],
          instructions: `Add import Lentils.${applet.name} to Lentils.lean alphabetically. Add dispatch entry and help text for ${applet.name} in Main.lean.`,
          agent: "implementer",
        });

        // Step 5: Build
        await context.call("build", {
          command: "lake build",
          agent: "tester",
        });

        // Step 6: Quick smoke test
        await context.call("test", {
          command: `.lake/build/bin/lentils ${applet.name} --help 2>&1`,
          agent: "tester",
        });

        results.push({ applet: applet.name, status: "done" });
      }
    }

    return {
      summary: `Implemented ${results.length} Tier-1 utilities`,
      milestones: plan.milestones.map(m => ({
        id: m.id,
        status: "done",
        applets: m.applets.map(a => a.name),
      })),
      newCount: results.length,
    };
  },
});
