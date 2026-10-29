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

## Import a public Git repository without a physical checkout

The JVM host supplies and owns a fresh initialized Datahike repository. The Git
transport writes files and exact Git objects directly into Geschichte; neither a
`.git` directory nor a native Git process is needed for HTTP import.

```clojure
(require '[geschichte.repo :as repo]
         '[geschichte.git.transport :as transport]
         '[geschichte.git.client :as client])

;; `conn` is an exclusively owned, initialized, empty Geschichte repository.
(def url "https://github.com/octocat/Hello-World.git")
(def imported
  (transport/clone! {:conn conn :remote "origin" :url url}))

(keys (repo/worktree conn))              ; => ("README")
(String. ^bytes (repo/read conn "README") "UTF-8")
(repo/configuration conn)                ; saved origin and branch upstream

;; Fetch changes without publishing them into the current worktree.
(def fetched
  (transport/fetch! {:conn conn :remote "origin" :url url
                     :options {:haves [(:git-oid imported)]}}))

;; Apply separately, after the host has checked its permissions and policy.
(client/apply-pull! conn "origin" fetched
                    {:policy :ff-only
                     :branch (subs (:ref imported) (count "refs/heads/"))})
```

`:haves` is an explicit protocol-v2 hint supported by both HTTP and SSH fetch.
Each supplied OID must name a commit whose **complete reachable object graph**
is still available in the selected object store. A successful full clone's tips
are suitable while retained. A remote-tracking ref alone, or a materialized
logical commit, is not proof of that availability. History materialization can
be bounded while the exact imported Git objects remain complete. Omitting
`:haves` preserves the full-fetch behavior; providing one branch's tip may still
transfer objects for other branches. Automatic negotiation is not implemented.
Callers own the connection/object-store lifetime and coordinate garbage
collection during these operations.

Fetch imports objects and updates remote-tracking refs; it preserves the local
worktree and index. Applying a pull requires a clean worktree/index and defaults
to fast-forward only. Divergence is refused rather than overwriting local work;
the lower-level API also supports explicit merge preparation. `:no-checkout?`
clones retain the remote configuration without selecting a branch upstream.
Clone failures can leave an unpublished partial repository; its host owns
cleanup, publication and retries. A clone is not a permission grant or an
instruction to load imported code.

`transport/clone!` chooses the ordinary JVM HTTP/SSH adapter and saves repository
configuration. For custom authentication or network policy, inject a host-owned
request function into `geschichte.git.http/clone!` / `fetch!` instead. These
lower-level transport functions do not configure the origin for the caller.
Keep credentials out of repository URLs/configuration. Embedded sandboxes must
not receive the default host transport implicitly. GitHub shorthand, connection
permissions, recurring synchronization, webhook handling and UI belong to the
embedding application.

## Incoming synchronization

`transport/pull!` uses the checked-out local branch's saved upstream when it
matches the requested remote. An explicit `:options {:branch "docs"}` overrides
that source selection. The remote source branch is independent of an optional
local destination `:ref`; changing the remote's default HEAD does not retarget an
existing clone. If the selected branch disappears from the fetched advertisement,
pull refuses rather than using a stale tracking ref or falling back to HEAD.

```clojure
(transport/pull! {:conn conn :remote "origin" :url url
                  :options {:policy :ff-only
                            :haves [(:git-oid imported)]}})
```

Success includes `:pull/status` of `:initialized`, `:up-to-date`, `:fast-forward`
or `:local-ahead`. The default policy does not merge divergent histories or push
local commits. Expected refusals expose `:pull/status` in `ex-data`:
`:local-changes`, `:diverged`, `:missing-branch` or `:unsupported-upstream`.
Fetch may update remote-tracking metadata even when applying the pull is refused;
the local worktree/index remain unchanged on these precondition refusals.

These APIs require exclusive caller ownership of the mutable repository during
application. Checkout/materialization can replace files in multiple transactions;
it is **not an atomic publication API for a concurrently edited live Memory**.
For a collaborative host, prepare downloads/checkouts in a privately owned
workspace and publish through a separately guarded merge/proposal operation.
Host permissions, source revision checks, cleanup, resource limits and incoming
sync scheduling remain explicit responsibilities. An exception after
materialization starts may leave partial private preparation; it must not be
advertised as a published sync or replayed blindly. No push authorization is
implied by fetching or successfully applying an incoming update.

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

## Checked JVM stream ingestion

`geschichte.content/transact-stream!` ingests a borrowed, finite binary
`InputStream` using content-defined chunks and bounded heap. Supply its exact
nonnegative byte size and a callback returning the metadata transaction data:

```clojure
(require '[geschichte.content :as content])

(with-open [source (clojure.java.io/input-stream finalized-file)]
  (content/transact-stream! conn source expected-size
    (fn [id] [{:attachment/id attachment-id
               :attachment/content [:geschichte.content/id id]}])
    {:expected-content inspected-content-id}))
```

The optional expected content UUID, complete hash, and actual byte count must
agree before the callback runs or metadata is published, even when the content
already exists. A source longer than declared consumes at most one extra byte
before refusal. The API does not close the caller's source. The caller retains
the source's exclusive finalized-file ownership and the database/GC lifetime
through the synchronous call. This does not provide an authority gate or a
worktree compare-and-swap: include the appropriate guard in the callback's
transaction data. Failure can leave unreferenced payloads for ordinary GC.
Existing file/byte ingestion APIs retain their representation-selection rules.

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
