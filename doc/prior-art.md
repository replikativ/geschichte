# Prior art and positioning

The space where version control meets databases is well-trodden. But each
neighbour occupies a *different* point than geschichte, and the specific
intersection geschichte targets — **real Git that is simultaneously a queryable
Datalog database** — came back empty from a fairly thorough survey. This note
maps the landscape and places geschichte in it.

The recurring distinction: geschichte stores *real Git* — genuine objects,
packs, refs, and the wire protocol, interoperable with native Git — but persists
them *inside* Datahike, so the Git object graph itself is queryable with Datalog
and graph algorithms. It is at once (a) a real Git implementation and (b) a
queryable database. Each category below is contrasted against that dual identity.

## 1. Git's object store backed by a database

libgit2's pluggable object-database backend (`git_odb_backend`, with a parallel
`git_refdb_backend`) is the mechanism that lets real Git objects live on a
foreign store, exposed through bindings like
[pygit2](https://www.pygit2.org/backends.html). Implementations:

- [libgit2-backends](https://github.com/libgit2/libgit2-backends) — SQLite /
  MySQL / Redis / memcached backends storing real objects keyed by SHA.
  Dormant (last real commit 2017; won't build against modern libgit2).
- [gitgres](https://github.com/andrew/gitgres) — a Postgres ODB+refdb backend;
  `push`/`clone` work against the DB, and it adds SQL views that parse
  commits/trees into rows. Active proof-of-concept (2026).
- [git-remote-sqlite](https://github.com/chrislloyd/git-remote-sqlite) — a
  remote helper writing loose objects into SQLite. Experimental ("toys only").
- [JGit](https://github.com/eclipse-jgit/jgit)'s `DfsObjDatabase` — the
  production way to put real objects (as *pack files*) into non-filesystem
  stores; the foundation under Gerrit and Google's Git infrastructure.

**Distinction:** these hold *real Git objects* but as a SHA-keyed blob store —
no rich query surface (gitgres and JGit are partial exceptions, offering SQL
views / pack storage, not a graph query over the object DAG). geschichte adds the
query layer these lack. (Note: production forges — GitHub Spokes/DGit, GitLab
Gitaly — keep objects as plain pack files on disk; their SQL DBs hold only
cluster metadata.)

## 2. A version-control system built on a database

- [Fossil](https://fossil-scm.org) — by SQLite's author; the entire repository
  is a single **SQLite database**, modelled as an immutable bag of SHA-hashed
  artifacts with relational tables as a derived index. Distributed, actively
  maintained, and it deliberately exposes SQL (`fossil sql`). The closest
  *philosophical* cousin — a DB-native VCS you can query with SQL — but it is
  **not Git** (no object/pack/wire interop) and queries its own derived schema,
  not the Git graph.
- [Monotone](https://www.monotone.ca) — the SQLite-backed predecessor that
  inspired Fossil; effectively dead (last release 2014).

## 3. "Git for data" — versioned databases with Git semantics

- [Dolt](https://github.com/dolthub/dolt) — a MySQL-compatible database with
  full Git-like versioning of tables (`commit`/`branch`/`merge`/`diff`). Storage
  is prolly-tree-based (from Noms), **not real Git objects**. Very active.
- [TerminusDB](https://github.com/terminusdb/terminusdb) — a versioned graph
  database with Git-for-data semantics and **WOQL, a Datalog-based query
  language**, over succinct immutable layers. Revived under DFRNT in 2025.
- [irmin](https://github.com/mirage/irmin) — the closest conceptual relative: an
  OCaml library for mergeable, branchable, distributed stores following Git's
  design, with pluggable backends. Its `irmin-git` backend writes a format real
  Git can inspect — the partial real-Git exception here — but it is a storage
  layer for irmin's own data model, with no query language.
- [Noms](https://github.com/attic-labs/noms) — content-addressed versioned DB
  that coined the "prolly tree"; archived (2021), its lineage feeds Dolt.
- Immutable / time-travel DBs — [XTDB](https://github.com/xtdb/xtdb),
  [Datomic](https://www.datomic.com), and
  [Datahike](https://github.com/replikativ/datahike) (geschichte's substrate) —
  give immutability and as-of/history, but not Git branch/merge or real Git
  objects.

**Distinction:** these reimplement Git-*like* semantics over bespoke storage,
with SQL or WOQL over their *own* data model and no real-Git interop.

## 4. Querying a Git repo read-only (Git as a data source)

- [MergeStat / askgit](https://github.com/mergestat/mergestat-lite) — SQL over a
  local repo via SQLite virtual tables (`commits`, `files`, `blame`). Stalled
  (last beta 2023).
- [gitql](https://github.com/filhodanuvem/gitql) — a SQL-like interpreter over a
  repo; nothing persisted. Stale.
- [gitbase](https://github.com/src-d/gitbase) (source{d}) — SQL over many repos
  via the MySQL wire protocol; dead/archived. The survivor is the actively
  maintained [go-git](https://github.com/go-git/go-git).
- [DuckDB `duck_tails`](https://duckdb.org/community_extensions/extensions/duck_tails)
  — Git-aware SQL; active.

**Distinction:** these are a transient lens held up to unchanged on-disk Git —
the query engine is never the system of record. You cannot transactionally
write, index, branch, or time-travel *through* the query layer. A Datalog engine
over local Git was found nowhere.

## 5. Content-addressed / Merkle-DAG storage

Git is itself a content-addressed Merkle DAG. Adjacent machinery:
[IPFS/IPLD](https://ipld.io) (a general Merkle-DAG data model; its
[go-ipld-git](https://github.com/ipfs/go-ipld-git) codec even represents real Git
objects as IPLD nodes, but purely as a non-queryable traversal codec), and
**prolly trees** (content-defined-chunked Merkle B-trees giving history
independence and diff-cost proportional to the change — the basis of Dolt and
ex-Noms). Complementary techniques geschichte could borrow, not a VCS.

## Where geschichte sits

- **Git ODB on a database** gives real objects but only a blob store.
- **A VCS on a database** (Fossil) is DB-native and SQL-queryable, but is not Git.
- **"Git for data"** (Dolt, TerminusDB, irmin) is Git-*like* over bespoke
  storage, not real Git.
- **Query git read-only** (askgit, gitql) is a transient lens, never the store.

geschichte is the only surveyed system that is *simultaneously* a faithful real
Git implementation and a first-class queryable database, where the Git object
graph *is* the primary store, expressed as datoms and interrogable with Datalog
and graph algorithms. The individual ingredients — pluggable ODBs, DB-native
VCS, Git-for-data semantics, prolly-tree sharing, content-addressed DAGs — are
all mature prior art; geschichte's contribution is the specific composition:
real Git rehoused inside an immutable Datalog database rather than mirrored,
approximated, or queried read-only.

*Caveats: "stalled"/"dead" labels are inferred from release cadence and archival
state, not formal shutdown notices. `irmin-git` and `go-ipld-git` are the
partial real-Git exceptions noted above. Project statuses are as of early 2026.*
