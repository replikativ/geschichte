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

For an immutable review ref, use create-only publication:

```clojure
(ws/publish! canonical agent {:ref "refs/heads/review-123"
                              :commit candidate-id :create-only? true})
```

Consumers can check `ws/publication-profile` before sending the new option to
a dynamically selected runtime; older libraries can ignore unknown options.
This requires the ref to be absent, including in the serialized publication
transaction. An existing unborn ref or the same tip also refuses; a concurrent
creator wins without its ref or metadata being overwritten. Successful return
includes `:created? true`. `:create? true` continues to allow either creation or
an ordinary fast-forward update. Create-only publication cannot force an update.

Publication transfers reachable immutable commit/content metadata and atomically
advances the canonical logical ref. It does not copy workspace-private config,
uncommitted files, staging state, or unrelated refs. Advance is explicit and
requires a clean target. Discard removes the physical Datahike branch; unreachable
payloads become eligible for garbage collection.

Publishing with `:create? true` may create a missing logical ref. Absence is
checked inside the same serialized transaction as its content and ref transfer;
an unforced competing creation refuses with
`:geschichte.workspace/publication-ref-exists` and leaves the winning state
untouched. Preparation captures one canonical snapshot. An existing ref's
name-to-entity identity is checked inside the writer before its target
compare-and-swap; deletion, rename or recreation refuses with
`:geschichte.workspace/stale-publication-ref`. This includes unborn refs whose
expected target is nil. An existing unborn ref is distinct from a missing ref.
`:force? true` deliberately permits
replacement, including a ref created after preparation; callers must authorize
that stronger policy explicitly. It addresses the current logical name rather
than a stale entity ID.

### Advance invariants

`advance!` observes one immutable canonical snapshot and updates only the target's
checked-out logical ref. It refuses pending merges and a different explicit
`:ref`; check out that branch first. Non-conflicting untracked files remain
untouched, while tracked files and the index become the selected committed tree.

Preparation does not write. The target's repository identity, physical workspace,
checkpoint, transaction number, refs, index and worktree are checked again inside
the serialized merge transaction, before any transfer or materialization datoms
are applied. Any intervening target transaction refuses with
`:geschichte.workspace/stale-advance`. The concurrent state remains intact;
callers must inspect it and prepare again rather than replay stale changes.

The source may advance independently: the returned `:canonical-commit`, imported
tree and retained merge ancestry all refer to the same captured source snapshot.
This operation does not promise the newest source state at completion. Callers
must keep both repositories and the captured history alive throughout the
operation; the prepared state is not a GC lease. Git transport pull/checkout has
its own exclusive-connection contract and does not acquire these guarantees.

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
