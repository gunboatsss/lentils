// smithers-source: custom
// smithers-display-name: Grep Regex
// smithers-description: Implement regex support for the grep utility.
/** @jsxImportSource smithers-orchestrator */
import { createSmithers } from "smithers-orchestrator";
import { z } from "zod/v4";
import { agents } from "../agents";

const featureResultSchema = z.looseObject({
  featureId: z.string().default("feature"),
  status: z.enum(["success", "partial", "failed"]).default("partial"),
  summary: z.string().default("Feature completed."),
  filesChanged: z.array(z.string()).default([]),
  commandsRun: z.array(z.string()).default([]),
  blockers: z.array(z.string()).default([]),
});

const inputSchema = z.object({
  goal: z.string().default(""),
  features: z.array(z.object({
    id: z.string(),
    title: z.string(),
    instructions: z.string(),
    files: z.array(z.string()).default([]),
    validation: z.array(z.string()).default([]),
  })).default([]),
});

const { Workflow, Task, Sequence, smithers, outputs } = createSmithers({
  input: inputSchema,
  featureResult: featureResultSchema,
});

const missionMemory = { kind: "workflow", id: "mission" } as const;

export default smithers((ctx) => {
  const features = ctx.input.features;
  const goal = ctx.input.goal;

  return (
    <Workflow name="grep-regex">
      <Sequence>
        {features.map((feature, i) => {
          const taskId = `feature:${feature.id}`;
          return (
            <Task
              key={taskId}
              id={taskId}
              output={outputs.featureResult}
              agent={agents.smart}
              timeoutMs={3_600_000}
              heartbeatTimeoutMs={900_000}
              continueOnFail
              memory={{ remember: { namespace: missionMemory, key: taskId } }}
            >
              {`You are implementing features for: ${goal}

FEATURE: ${feature.title}
${feature.instructions}

FILES: ${feature.files.join(", ")}

VALIDATION: ${feature.validation.join("; ")}

After completing this feature, output JSON with:
{
  "featureId": "${feature.id}",
  "status": "success" | "partial" | "failed",
  "summary": "What you did",
  "filesChanged": ["list"],
  "commandsRun": ["list"],
  "blockers": ["any issues"]
}

IMPORTANT: Never run scripts/install-symlinks.sh. Use lake build to verify.`}
            </Task>
          );
        })}
      </Sequence>
    </Workflow>
  );
});
