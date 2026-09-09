---
name: subagent-dispatch
description: Use whenever you're about to hand a one-off task to a subagent (implement, review, or spec-check) and no model has already been chosen for it in this conversation. Asks the user which model to use, then dispatches via the `subagent` tool. Not for the Animoid pipeline (which pins specific models per stage — see animoid-full-pipeline) or the copilot plan-implement-review-remediate loop (see copilot-plan-implement-pipeline, which already does this for its implementer role).
---

# Subagent dispatch (ask first, then delegate)

Do NOT silently use whatever pi's default model is when spawning a plain,
generic subagent (`implementer`, `code-reviewer`, `spec-reviewer`, or a
similar one-off task with no project-specific pipeline in play). Ask the
user which model to use, generate a per-run agent file with that model
pinned, then dispatch with `subagent`.

If the user's instruction already names a bare model/tier word instead of
asking you to choose ("use subagent luna", "dispatch this on sol", "spin up
an opus reviewer") — **read `model-provider-routing` first**, before step 2
below. That word names a model, not a provider, and several providers in
this install expose the exact same model id. Resolve `<provider>/<model-id>`
using that skill's rules (current session provider by default, verified via
`pi --list-models`) instead of calling `ask_model` or guessing a provider
from whatever similarly-named agent file you happen to find first.

This is the same pattern `copilot-plan-implement-pipeline` and
`animoid-full-pipeline` already use for their user-chosen-model stages —
this skill just makes it available for any standalone subagent call, not
only inside those larger pipelines.

## Steps

1. Choose the stable role profile: `implementer`, `code-reviewer`, or
   `spec-reviewer`.
2. Resolve `CHOSEN_MODEL` with `ask_model` (or `model-provider-routing` for a
   user-named bare tier). Derive optional `CHOSEN_THINKING` when applicable.
3. Dispatch directly—do not generate or rewrite an agent file:

   ```ts
   subagent({ agent: "<role>", model: CHOSEN_MODEL, thinking: CHOSEN_THINKING, task: "<full task>" })
   ```

   Omit `thinking` when the model has no thinking-level concept.
4. Verify the resulting diff and tests yourself; a subagent report is not proof.

## When NOT to use this skill

- Any Animoid work at all, pipeline or one-off — see `animoid/SKILL.md`'s
  "One-off Animoid dispatches" section. Animoid has its own five fixed
  stable profiles (implement/remediate/review/plan/investigate) in
  `animoid-full-pipeline/`; always reuse one of those instead of this
  skill's step 1 fallback ("write a new one-off agent file yourself if none
  fit") — that fallback is exactly what produced the stray
  `animoid-*`/`openai-codex-*-animoid-*`/`qwen-*-animoid-*` one-off agent
  files that used to clutter `~/.pi/agent/agents/`.
- Inside `copilot-plan-implement-pipeline` — it already asks per round via
  its own `ask_model` + direct `implementer` dispatch; don't duplicate
  the prompt.
- If the user already told you which model to use for this exact task in
  the current turn — just use it, no need to ask again.
