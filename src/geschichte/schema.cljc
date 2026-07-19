(ns geschichte.schema
  "Datahike schema for Geschichte repositories.")

(def schema
  [;; Repository singleton. The symbolic head names a logical Geschichte ref;
   ;; Datahike's own :config/:branch remains the isolated workspace branch.
   {:db/ident :geschichte.repo/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.repo/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.repo/head
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.repo/merge-parent
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   ;; Repository-local Git compatibility and Geschichte settings. Global host
   ;; configuration remains outside the repository and is merged at the CLI
   ;; boundary; local values must survive separate native CLI invocations.
   {:db/ident :geschichte.config/key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.config/value
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}

   ;; Logical branches/tags/remotes. Ref names are Git-shaped strings rather
   ;; than Datahike branch keywords because the two branch layers are distinct.
   {:db/ident :geschichte.ref/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.ref/target
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   ;; Explicit user-visible commits select durable Datahike checkpoints. A
   ;; future Datahike :db.type/commit-ref will replace the UUID once pinned
   ;; snapshot roots are supported by GC and replication.
   {:db/ident :geschichte.commit/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.commit/snapshot
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.commit/parents
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}
   {:db/ident :geschichte.commit/message
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.commit/author
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.commit/time
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.commit/git-oid
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}

   ;; Exact Git objects are pack-backed. Lookup metadata is split by the first
   ;; OID nibble so only one bounded index shard is loaded for an object lookup
   ;; without paying hundreds of store writes for ordinary repositories.
   {:db/ident :geschichte.git.pack/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.git.pack/size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.pack/checksum
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.pack/index-shards
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}
   {:db/ident :geschichte.git.pack/chunks
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}
   {:db/ident :geschichte.git.pack/object-format
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.index/prefix
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.index/count
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.index/payload
    :db/valueType :db.type/store-ref
    :db/cardinality :db.cardinality/one}

   ;; Remote/imported refs point directly at Git OIDs. They are distinct from
   ;; Geschichte refs whose targets are materialized logical commits.
   {:db/ident :geschichte.git.ref/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.git.ref/source
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.ref/oid
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.ref/peeled
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.git.ref/symref-target
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}

   ;; Logical content is separate from physical representation. Its stable ID
   ;; hashes reconstructed bytes; payload can be a full byte string or a bounded
   ;; delta, and base refs make the reconstruction graph visible to Datahike.
   {:db/ident :geschichte.content/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.content/kind
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/payload
    :db/valueType :db.type/store-ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/base
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/depth
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/chunking-algorithm
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/chunking-version
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/chunk-min-size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/chunk-size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/chunk-max-size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.content/chunks
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}

   ;; Chunk order is explicit because cardinality-many refs are a set. Each
   ;; payload remains a first-class store-ref, making GC and Datahike's Kabel
   ;; reachability walker see exactly the immutable Konserve objects to retain
   ;; and replicate.
   {:db/ident :geschichte.chunk/index
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.chunk/offset
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.chunk/payload
    :db/valueType :db.type/store-ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.chunk/size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}

   ;; Working tree: path identity is workspace-local. File identity and rename
   ;; tracking will be added above this representation; exact path snapshots are
   ;; the smallest slice that exercises staging/checkout and Git projection.
   {:db/ident :geschichte.work/path
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.work/content
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.work/size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.work/mode
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}

   ;; Empty directories are workspace state, not commit-tree state (matching
   ;; Git, whose trees cannot represent empty directories). Non-empty parent
   ;; directories can always be derived from file paths; this attribute makes
   ;; mkdir/cd useful before the first file is written.
   {:db/ident :geschichte.work-dir/path
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}

   ;; The index is a full path map. A missing worktree path staged from an
   ;; existing index entry becomes :deleted; commit snapshots retain that fact.
   {:db/ident :geschichte.stage/path
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :geschichte.stage/state
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.stage/content
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.stage/size
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :geschichte.stage/mode
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}])
