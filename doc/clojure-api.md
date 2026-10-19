# Clojure API

Geschichte deliberately exposes small namespaces rather than one generated API.

| Namespace | Responsibility |
|---|---|
| `geschichte.repo` | repository lifecycle, worktree/index, commits, refs, merge and history |
| `geschichte.fs` | directory-aware byte filesystem over the current worktree |
| `geschichte.workspace` | structurally shared Datahike workspaces and publication |
| `geschichte.query` | portable read views over Datahike database values |
| `geschichte.git.local` | native `.git` discovery and object/ref import |
| `geschichte.git.http` | injectable smart-HTTP clone/fetch/push/pull |
| `geschichte.git.transport` | JVM HTTP/SSH transport selection |

## Filesystem API

```clojure
(require '[geschichte.fs :as fs])

(fs/mkdir! conn "/src")
(fs/write! conn "/src/demo.clj" (.getBytes "(ns demo)\n" "UTF-8"))
(fs/stat conn "/src/demo.clj")
(fs/list-dir conn "/src")
(fs/rename! conn "/src/demo.clj" "/src/core.clj")
(fs/delete! conn "/src/core.clj")
```

Directory entities are worktree state. Git/Geschichte commits contain files;
empty directories are not committed.

## Datalog

The query namespace returns stable maps for common UI/application needs, while
ordinary Datahike queries can combine repository facts with application data:

```clojure
(require '[datahike.api :as d]
         '[geschichte.query :as gq])

(gq/repository @conn)
(gq/refs @conn)
(gq/commits @conn)
(gq/worktree @conn)

(d/q '[:find ?path ?size
       :where
       [?entry :geschichte.work/path ?path]
       [?entry :geschichte.work/size ?size]]
     @conn)
```

Payload bytes are intentionally store refs, not indexed inline values. Query
metadata first and load bytes through `repo/read`, `repo/read-entry`, or Konserve
blob access only when required.

## Atomic publication with application receipts

`prepare-commit` captures one immutable repository DB after staging. It returns
a short-lived plan for an ordinary commit or a validated no-op; it does not
transact. Amend and pending merges are refused.

Call `prepared-commit-tx` **inside** a Datahike transaction function and append
application receipt data to its returned vector. Commit, logical ref, and receipt
then publish together. For idempotent recovery, look up the operation receipt
first: a duplicate must return the winning receipt without revalidating an old
plan. Authorization still belongs to the host application.

The plan binds its store identity and physical workspace branch, not only the
logical Git ref: a freshly forked workspace can share the same checkpoint.
The first API deliberately requires the exact physical Datahike checkpoint and
transaction number to remain current. The latter also distinguishes provisional
transactions within a writer batch that still share checkpoint metadata.
Any intervening transaction invalidates the plan, including
unrelated metadata changes. Finish schema, file writes, staging, and application
metadata before preparing. Existing unstaged files are preserved. A no-op returns
no commit/ref datoms, but still validates the same basis and can carry a receipt.
Plans are not durable queued work or history-retention pins; do not persist one
and assume its historical content survives future garbage collection.

## Async portability

JVM calls return direct values. Effectful CLJS calls return partial-CPS awaitable
values where Datahike/Konserve are asynchronous; pure query, diff, merge-planning,
Git object, and protocol operations remain synchronous.
