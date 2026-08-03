---
name: absurd
description: Debug and operate Absurd durable workflows with absurdctl. Use when the user explicitly mentions Absurd or absurdctl, or clearly asks about Absurd workflow state such as queues, tasks, runs, sleeping tasks, events, or retries in that context. Do not use for generic queue, event, retry, or task-system questions.
license: Apache-2.0
---

# Absurd

Use this skill only for **Absurd-specific** workflow operations and debugging.

## Boundaries
- Use **this** skill when the user explicitly means the **Absurd** durable workflow engine or **`absurdctl`**.
- Do **not** use this skill for generic background-job, queue, retry, event, or task questions unless the Absurd context is explicit.

Use this skill when the project uses **Absurd**, the Postgres-native durable workflow engine, or when the user mentions **`absurdctl`**, Absurd queues, durable tasks, runs, retries, sleeping tasks, events, `spawn-task`, `emit-event`, `retry-task`, `cancel-task`, `dump-task`, `list-queues`, or `list-tasks`.

## Tiny mental model

- A **queue** is a namespace of Absurd tables (`t_`, `r_`, `c_`, `e_`, `w_`).
- A **task** is the durable workflow instance.
- A **run** is one execution attempt of a task.
- A **step** is a checkpoint. Completed step results are stored as JSON.
- **Sleeping** tasks are usually waiting for time or an event.
- **Events** wake waiting tasks. Event payloads are cached; first emit wins.

Important distinction:

- `task_id` = the whole workflow across all attempts
- `run_id` = one specific execution attempt

## First: make sure the local Absurd DB is reachable, then make sure `absurdctl` works

Before running any `absurdctl` command against the default local database, first check whether local Postgres is reachable on `localhost:5432`.

- If it is reachable, continue.
- If it is not reachable, bootstrap it using the **OS-aware local bootstrap flow below** before attempting any `absurdctl` command.
- Do not start debugging workflow state against a dead local database.

If `absurdctl` is not on `PATH`, check whether you are inside a repo checkout and, if so, use:

```bash
export PATH="$PWD:$PATH"
```

Absurd connection precedence is:

```text
--database > ABSURD_DATABASE_URL > PGDATABASE > postgresql://localhost/absurd
```

For non-URI connections, `PGHOST`, `PGPORT`, `PGUSER`, and `PGPASSWORD` are also honored.

## Local Absurd database bootstrap (OS-aware)

If the local Absurd Postgres is not running, choose the bootstrap path by OS:

- **macOS / Darwin** → use **Apple `container`** only
- **Linux** → use **Podman** (lightweight Linux containers)
- **Windows** → unsupported here; do not attempt to bootstrap

The file under:

```text
~/git/newsuk/general/pg/podman-compose.yaml
```

is only a configuration reference for the desired Postgres settings:

- image: `postgres:16`
- container name: `absurd-postgres`
- host port: `5432`
- database: `absurd`
- user: `postgres`
- password: `postgres`
- persistent data volume for `/var/lib/postgresql/data`
- on Apple `container` with a named volume, set `PGDATA=/var/lib/postgresql/data/pgdata` to avoid the Postgres `lost+found` mount-point initialization issue

Do **not** use Docker. Do **not** use Compose. On macOS, do **not** use Podman either.

### Preferred startup flow

1. Check whether Postgres is already reachable:

```bash
pg_isready -h 127.0.0.1 -p 5432 -U postgres -d absurd
```

If `pg_isready` is unavailable, use:

```bash
nc -z 127.0.0.1 5432
```

2. Detect platform:

```bash
uname -s
```

3. On **Darwin**, use Apple `container`:

```bash
container list --all
container start absurd-postgres
# or, if it does not exist yet:
container image pull postgres:16
container volume create absurd-postgres-data
container run -d \
  --name absurd-postgres \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_DB=absurd \
  -e PGDATA=/var/lib/postgresql/data/pgdata \
  -p 5432:5432 \
  -v absurd-postgres-data:/var/lib/postgresql/data \
  postgres:16
container exec absurd-postgres pg_isready -U postgres -d absurd
pg_isready -h 127.0.0.1 -p 5432 -U postgres -d absurd
```

If needed, inspect logs:

```bash
container logs absurd-postgres
```

4. On **Linux**, use Podman:

```bash
podman ps -a
podman start absurd-postgres
# or, if it does not exist yet:
podman pull postgres:16
podman volume create absurd-postgres-data
podman run -d \
  --name absurd-postgres \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_DB=absurd \
  -p 5432:5432 \
  -v absurd-postgres-data:/var/lib/postgresql/data:Z \
  postgres:16
podman exec absurd-postgres pg_isready -U postgres -d absurd
```

If needed, inspect logs:

```bash
podman logs absurd-postgres
```

5. Then proceed with `absurdctl`.

### Operational notes

- Prefer reusing the existing container name: `absurd-postgres`
- Prefer reusing the existing volume name: `absurd-postgres-data`
- If the user asks to reset local state, stop and delete the container, then delete the named volume and recreate it
- On macOS, if Apple `container` is unavailable, stop and tell the user the local Absurd database cannot be bootstrapped in the required Apple-container-only mode
- On Linux, if Podman is unavailable, stop and tell the user the local Absurd database cannot be bootstrapped in the required Docker-free Linux mode
- On macOS Apple `container`, host forwarding may be IPv4-only; if `localhost` fails after startup, use `127.0.0.1` explicitly via `PGHOST=127.0.0.1` or `ABSURD_DATABASE_URL=postgresql://postgres:postgres@127.0.0.1:5432/absurd`

### One-shot: ensure local Absurd DB is up

Use this exact flow when the user wants the local Absurd database started or verified:

```bash
if command -v pg_isready >/dev/null 2>&1; then
  pg_isready -h 127.0.0.1 -p 5432 -U postgres -d absurd && exit 0
else
  nc -z 127.0.0.1 5432 && exit 0
fi

case "$(uname -s)" in
  Darwin)
    container list --all
    if container list --all | grep -q 'absurd-postgres'; then
      container start absurd-postgres || true
    else
      container image pull postgres:16
      container volume create absurd-postgres-data || true
      container run -d \
        --name absurd-postgres \
        -e POSTGRES_USER=postgres \
        -e POSTGRES_PASSWORD=postgres \
        -e POSTGRES_DB=absurd \
        -e PGDATA=/var/lib/postgresql/data/pgdata \
        -p 5432:5432 \
        -v absurd-postgres-data:/var/lib/postgresql/data \
        postgres:16
    fi
    until container exec absurd-postgres pg_isready -U postgres -d absurd; do
      sleep 1
    done
    until pg_isready -h 127.0.0.1 -p 5432 -U postgres -d absurd; do
      sleep 1
    done
    ;;
  Linux)
    podman ps -a
    if podman ps -a --format '{{.Names}}' | grep -q '^absurd-postgres$'; then
      podman start absurd-postgres || true
    else
      podman pull postgres:16
      podman volume create absurd-postgres-data || true
      podman run -d \
        --name absurd-postgres \
        -e POSTGRES_USER=postgres \
        -e POSTGRES_PASSWORD=postgres \
        -e POSTGRES_DB=absurd \
        -p 5432:5432 \
        -v absurd-postgres-data:/var/lib/postgresql/data:Z \
        postgres:16
    fi
    until podman exec absurd-postgres pg_isready -U postgres -d absurd; do
      sleep 1
    done
    ;;
  *)
    echo 'Unsupported OS for local Absurd DB bootstrap' >&2
    exit 1
    ;;
esac
```

After that, continue with `absurdctl`.

### Reset playbook: recreate the local Absurd DB

Only do this when the user explicitly wants local state destroyed and recreated:

```bash
case "$(uname -s)" in
  Darwin)
    container stop absurd-postgres || true
    container delete --force absurd-postgres || true
    container volume delete absurd-postgres-data || true
    container volume create absurd-postgres-data
    container run -d \
      --name absurd-postgres \
      -e POSTGRES_USER=postgres \
      -e POSTGRES_PASSWORD=postgres \
      -e POSTGRES_DB=absurd \
      -e PGDATA=/var/lib/postgresql/data/pgdata \
      -p 5432:5432 \
      -v absurd-postgres-data:/var/lib/postgresql/data \
      postgres:16
    until container exec absurd-postgres pg_isready -U postgres -d absurd; do
      sleep 1
    done
    until pg_isready -h 127.0.0.1 -p 5432 -U postgres -d absurd; do
      sleep 1
    done
    ;;
  Linux)
    podman stop absurd-postgres || true
    podman rm -f absurd-postgres || true
    podman volume rm absurd-postgres-data || true
    podman volume create absurd-postgres-data
    podman run -d \
      --name absurd-postgres \
      -e POSTGRES_USER=postgres \
      -e POSTGRES_PASSWORD=postgres \
      -e POSTGRES_DB=absurd \
      -p 5432:5432 \
      -v absurd-postgres-data:/var/lib/postgresql/data:Z \
      postgres:16
    until podman exec absurd-postgres pg_isready -U postgres -d absurd; do
      sleep 1
    done
    ;;
  *)
    echo 'Unsupported OS for local Absurd DB reset' >&2
    exit 1
    ;;
esac
```

## Default debugging workflow

Prefer **`absurdctl` state inspection before source inspection**. Usually you do **not** need to read application code first.

But first, if the task is targeting the local default database, verify `localhost:5432` is up. If it is down, run the OS-aware bootstrap flow in this skill before any `absurdctl list-queues`, `list-tasks`, `dump-task`, or other inspection command.

If the user explicitly asks you to use **`absurdctl`** to inspect or fix a workflow, do that first instead of starting with `rg` / source browsing.

When the user wants to debug a task, start with these commands in order:

### 1) Discover queues

```bash
absurdctl list-queues
```

### 2) Inspect recent activity in the likely queue

```bash
absurdctl list-tasks --queue=default --limit=20
```

Notes:
- `list-tasks` defaults to 50 rows if `--limit` is omitted.
- Useful statuses: `pending`, `running`, `sleeping`, `completed`, `failed`, `cancelled`.

### 3) Focus on failures or sleepers

```bash
absurdctl list-tasks --queue=default --status=failed --limit=20
absurdctl list-tasks --queue=default --status=sleeping --limit=20
```

### 4) Inspect one workflow or one attempt in detail

```bash
absurdctl dump-task --task-id=<task-id>
absurdctl dump-task --run-id=<run-id>
```

`dump-task` is the most important inspection command. It shows things like:
- task name, params, and headers
- retry settings and attempts
- checkpointed step state
- waits / events / sleep state
- final result or failure

## How to reason about common states

### If a task is `failed`

1. `dump-task --task-id=<task-id>`
2. Read the failure and the last successful checkpoints.
3. If needed, inspect the most recent attempt with `--run-id`.
4. Search the code for the task implementation by task name.
5. Only then decide whether to retry.

### If a task is `sleeping`

1. `dump-task --task-id=<task-id>`
2. Look for the wait reason:
   - sleeping until a timestamp
   - waiting for an event name
3. If it is waiting for an event and the user wants it resumed, emit that event.

### If a task is `running`

1. `dump-task --task-id=<task-id>`
2. Look at existing checkpoints to see how far it got.
3. If the user suspects a stuck worker, inspect the worker process / application logs too.

### About workers

Do not assume you need to start or modify a worker.

- If a spawned task moves from `pending` to `sleeping`, `running`, or `completed`, a worker is already active.
- If tasks remain `pending`, then investigate whether a worker for that queue is actually running.
- Only inspect Python / TypeScript runtime details when the task state suggests a worker problem or the user asks for code changes.

### If you need the implementation

After you know the task name, search the codebase for its registration.

TypeScript / JavaScript:

```bash
rg -n "registerTask\(|name:\s*['\"]<task-name>['\"]" .
```

Python:

```bash
rg -n "register_task\(|@.*register_task|['\"]<task-name>['\"]" .
```

If the task is waiting for an event, also search for the event name.

## Common actions

### Spawn work

Use `-P key=value` for strings and `-P key:=json` for typed JSON values.

```bash
absurdctl spawn-task my-task -q default -P foo=bar
absurdctl spawn-task my-task -q default -P count:=42 -P enabled:=true
absurdctl spawn-task my-task -q default -P user.name=Alice -P user.age:=30
```

Use `--params` when the user already has a JSON object:

```bash
absurdctl spawn-task my-task -q default --params '{"foo":"bar","count":42}'
```

### Retry failed work

```bash
absurdctl retry-task <task-id>
absurdctl retry-task <task-id> --max-attempts 5
absurdctl retry-task -q default <task-id> --spawn-new
```

Guidance:
- plain `retry-task` retries the existing task
- `--spawn-new` creates a brand-new task with the original inputs
- prefer understanding the failure before retrying

### Cancel work

```bash
absurdctl cancel-task <task-id>
absurdctl cancel-task -q default <task-id>
```

### Wake waiting tasks by emitting an event

```bash
absurdctl emit-event order.completed -q default -P orderId=123
absurdctl emit-event approval.granted:42 -q default -P approved:=true
```

If the event payload should be structured JSON:

```bash
absurdctl emit-event shipment.packed:42 -q default --payload '{"trackingNumber":"XYZ"}'
```

### Schema setup / migrations

Use these on a blank or controlled database, or when the user explicitly asks:

```bash
absurdctl init
absurdctl schema-version
absurdctl migrate
absurdctl create-queue default
```

## Safe operating rules

Be careful with state-changing commands. Unless the user clearly wants them, avoid running these blindly on a shared or production database:

- `init`
- `migrate`
- `create-queue`
- `drop-queue`
- `cleanup`
- `cancel-task`
- `retry-task`
- `emit-event`
- `spawn-task`

If the environment is ambiguous, ask which database / queue is safe to operate on.

## Good copy-paste playbooks

### Debug the latest failures in `default`

```bash
absurdctl list-queues
absurdctl list-tasks --queue=default --status=failed --limit=20
absurdctl dump-task --task-id=<task-id>
```

### Find sleepers and wake one

```bash
absurdctl list-tasks --queue=default --status=sleeping --limit=20
absurdctl dump-task --task-id=<task-id>
absurdctl emit-event <event-name> -q default -P key=value
```

### Reproduce by spawning a task, then inspect it

```bash
absurdctl spawn-task my-task -q default -P foo=bar
absurdctl list-tasks --queue=default --task-name=my-task --limit=5
absurdctl dump-task --task-id=<task-id>
```

Fast path when the user says “spawn a task and debug it”:

```bash
absurdctl spawn-task my-task -q default -P foo=bar
absurdctl list-tasks --queue=default --task-name=my-task --limit=5
absurdctl dump-task --task-id=<task-id>
# then either:
absurdctl emit-event <event-name> -q default -P key=value
# or:
absurdctl retry-task <task-id>
```

## Extra reference

- Use `absurdctl <command> --help` for full options.
- `dump-task --task-id` is usually the best starting point once you know the task.
- Checkpointed step results are durable JSON state; code outside steps may execute multiple times across retries.
