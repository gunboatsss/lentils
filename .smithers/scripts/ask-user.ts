#!/usr/bin/env bun
/**
 * ask-user.ts — Block until user answers a question.
 * Usage: bun ask-user.ts "Your question here" --recommended "default answer"
 */
import { createInterface } from "readline/promises";
import { stdin as input, stdout as output } from "process";

const args = process.argv.slice(2);
const question = args[0];
const recommendedIdx = args.indexOf("--recommended") !== -1 ? args.indexOf("--recommended") : args.indexOf("-r");
const recommended = recommendedIdx !== -1 ? args[recommendedIdx + 1] : undefined;

const rl = createInterface({ input, output });
let prompt = question;
if (recommended) prompt += ` [${recommended}]`;
prompt += ": ";

const answer = await rl.question(prompt);
rl.close();
console.log(answer || recommended || "");
