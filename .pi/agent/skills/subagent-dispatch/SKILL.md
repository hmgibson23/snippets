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

1. Decide the role: `implementer`, `code-reviewer`, or `spec-reviewer` (or
   pick the closest match; write a new one-off agent file yourself if none
   fit — same shape: frontmatter `name`/`description`/`tools`/`model:
   {MODEL}`/`timeout`, then a system prompt body).

2. Ask:

   ```
   ask_model(reason = "to <implement/review/spec-check> <short task description>")
   ```

   Use the returned string as `CHOSEN_MODEL`. If the user cancels or says
   "keep using current model", use the current session model instead — do
   not re-ask. **Skip this step** if `model-provider-routing` already gave
   you a confirmed `<provider>/<model-id>` from the user's own wording —
   use that as `CHOSEN_MODEL` directly instead of asking again.

3. Read the matching template in this skill directory:
   - `implementer-agent.template.md` → generic-implementer-dynamic
   - `code-reviewer-agent.template.md` → generic-code-reviewer-dynamic
   - `spec-reviewer-agent.template.md` → generic-spec-reviewer-dynamic

4. Replace `{MODEL}` with `CHOSEN_MODEL` and write the result to
   `~/.pi/agent/agents/<template-name>.md` (e.g.
   `~/.pi/agent/agents/generic-implementer-dynamic.md`). This overwrites any
   previous one-off run — that's fine, it's per-run scratch, not something to
   preserve.

5. Call `subagent` with that dynamic agent name and the real task text
   (agent + task, single mode). Do not use `parallel`/`chain` unless you
   genuinely have independent tasks to fan out — ask the user's model choice
   applies per call, so for multiple independent one-off tasks with
   different desired models, ask once per distinct role/task before
   dispatching each.

6. Do not trust the subagent's self-reported "done" — it's a summary from a
   real headless `pi` subprocess with no memory of this conversation, not
   proof. Verify with `git diff`/tests yourself, same as any other subagent
   dispatch.

## When NOT to use this skill

- Inside `animoid-full-pipeline` — its three roles (deepseek implementer,
  terra remediator, sol reviewer) are intentionally pinned; never substitute
  a different model for them without the user explicitly saying so.
- Inside `copilot-plan-implement-pipeline` — it already asks per round via
  its own `ask_model` + `implementer-agent.template.md` step; don't duplicate
  the prompt.
- If the user already told you which model to use for this exact task in
  the current turn — just use it, no need to ask again.
