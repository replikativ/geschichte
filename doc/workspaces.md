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
