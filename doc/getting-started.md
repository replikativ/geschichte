# Getting started

Geschichte repositories are Datahike databases. A connection contains a mutable
worktree, a staging index, logical refs, explicit commits, and immutable content.

## Create a repository

```clojure
(require '[datahike.api :as d]
         '[geschichte.repo :as repo])

(def cfg {:store {:backend :file :path "/tmp/example-geschichte"
                  :id (random-uuid)}
          :schema-flexibility :write
          :keep-history? true
          :commit-graph? true})

(d/create-database cfg)
(def conn (d/connect cfg))
(repo/init! conn {:name "example"})
```

The store must support Datahike's raw blob API. File payloads are stored through
Konserve and referenced from indexed repository metadata with store refs.

## Write, inspect, and commit

```clojure
(repo/write! conn "src/demo.clj" (.getBytes "(ns demo)\n" "UTF-8"))
(repo/status conn)
(repo/stage! conn ["src/demo.clj"])
(repo/commit! conn {:message "Add demo namespace"
                    :author "Ada <ada@example.test>"})
(repo/log conn {:limit 10})
```

Writes and staging operations are durable Datahike checkpoints. Only
`repo/commit!` creates a user-visible Geschichte commit and advances the current
logical ref.

## Read files and historical trees

```clojure
(String. ^bytes (repo/read conn "src/demo.clj") "UTF-8")
(repo/tree conn :worktree)
(repo/tree conn :index)
(repo/tree conn :head)
(repo/changes conn :head :worktree)
```

Use `d/release` when finished. For independent agent/session workspaces, continue
with [Workspaces and publication](workspaces.md).
