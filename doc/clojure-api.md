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
| `geschichte.yggdrasil` | optional Yggdrasil adapter |

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

## Async portability

JVM calls return direct values. Effectful CLJS calls return partial-CPS awaitable
values where Datahike/Konserve are asynchronous; pure query, diff, merge-planning,
Git object, and protocol operations remain synchronous.
