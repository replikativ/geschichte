# Workspaces and publication

Geschichte separates two concepts that Git worktrees couple:

- a **Datahike workspace branch** is an isolated mutable worktree/index/ref catalog;
- a **logical Geschichte ref** such as `refs/heads/main` is versioned repository data.

Consequently many agents can independently check out logical `main`. Their
Datahike branches share persistent index structure and immutable payloads.

```text
canonical :db (publication authority)
       ├── workspace agent-a → logical main
       ├── workspace agent-b → logical main
       └── workspace browser → logical main
```

```clojure
(require '[geschichte.workspace :as ws])

(ws/fork! canonical "agent-a")
(def agent (d/connect (assoc cfg :branch (ws/branch-key "agent-a"))))

;; edit and commit through agent
(ws/publish! canonical agent)
(ws/advance! canonical another-clean-workspace)
(ws/remove! canonical "agent-a")
```

Publication transfers reachable immutable commit/content metadata and atomically
advances the canonical logical ref. It does not copy workspace-private config,
uncommitted files, staging state, or unrelated refs. Advance is explicit and
requires a clean target. Discard removes the physical Datahike branch; unreachable
payloads become eligible for garbage collection.

The optional Yggdrasil adapter maps fork/merge/discard onto this model and lets
Geschichte participate in Spindel execution contexts.

## Exact database checkpoint origins

A logical commit identifies a committed tree. An exact workspace checkpoint
also preserves dirty files, staging, refs, schema and other repository datoms.
Its portable identity is repository-specific:

```clojure
(def origin {:version 1 :kind :datahike :commit datahike-commit-uuid
             :source {:store store-uuid :repository repository-uuid}})
(def snapshot (ws/resolve-checkpoint canonical origin))
;; Establish native allocation/cleanup ownership before calling the allocator.
(def branch (ws/fork-at-checkpoint! canonical "historical" origin snapshot))
(def child (d/connect (assoc cfg :branch branch)))
(ws/verify-checkpoint-workspace! snapshot @child branch)
;; Use child; then release its connection and remove the owned branch.
```

Resolution is read-only and validates source identity and checkpoint availability
before allocation. The allocator rechecks source/snapshot correspondence and
returns immediately after the named branch allocation acknowledges; it does not
connect or perform fallible post-allocation work. A lifecycle owner can therefore
record that ACK, connect the child, and verify it before exposing the workspace.
An uncertain allocation must stay owned rather than being replayed or assumed
absent. Destination collisions preserve the existing branch.

Verification compares a constant number of immutable primary index roots,
secondary generation addresses, source configuration and database metadata.
It reads no file bodies and leaves the source untouched. Building secondary
indices refuse because Datahike omits unfinished generations when branching.
JVM operations return direct values and CLJS operations return CPS computations;
explicit unsupported execution modes refuse before source IO.

The origin and resolved immutable DB are neither permission nor a GC lease.
Callers retain the source and evidence for the promised lifetime. The child is
writable; its physical branch identity differs intentionally. Logical publication
and merge retain their existing file/ref semantics. Selecting one repository
checkpoint does not rewind other repositories or a computation's execution state.

Yggdrasil delegates these repository operations while owning its overlay lifecycle
and cross-system capability/preflight checks. No reverse dependency on Yggdrasil
is required to use Geschichte checkpoints.
