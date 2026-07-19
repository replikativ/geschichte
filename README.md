# geschichte

[![Slack](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/CB7GJAN0L)
[![Clojars](https://img.shields.io/clojars/v/org.replikativ/geschichte.svg)](https://clojars.org/org.replikativ/geschichte)
[![CircleCI](https://circleci.com/gh/replikativ/geschichte.svg?style=shield)](https://circleci.com/gh/replikativ/geschichte)
[![last-commit](https://img.shields.io/github/last-commit/replikativ/geschichte/main.svg)](https://github.com/replikativ/geschichte)

**Git, as a queryable database.**

geschichte is version control whose objects, refs, worktree, and history live in
[Datahike](https://github.com/replikativ/datahike) and
[Konserve](https://github.com/replikativ/konserve) — so you can commit, branch,
and merge with Git semantics, query the whole repository with Datalog, and
replicate it over the [replikativ](https://github.com/replikativ) stack. It reads
and writes real Git objects, packs, and the wire protocol without shelling out to
native Git, and the same repository can back a virtual filesystem, the `ges`
CLI, or [Muschel](https://github.com/replikativ/muschel)'s integrated `git`
command. Real Git that is also a queryable database is, to our knowledge, a
combination no other system provides — see [Prior art](doc/prior-art.md).

It is a new implementation in the lineage of the original *geschichte* project,
which became replikativ.

> **Beta.** geschichte is usable but pre-1.0. On-disk and database formats may
> still change between releases; you may need to re-import or migrate
> repositories as it stabilises.

## Why geschichte?

- 🗃️ **[Queryable history](doc/querying.md)** — repository state is ordinary
  Datahike data: ask Datalog for every path, every commit, every ref, run graph
  algorithms over the commit DAG, and join it all against another repository or
  your application's own data.
- 🌿 **[Workspaces, not worktrees](doc/workspaces.md)** — many isolated
  worktrees may independently share one logical branch name, and every write is
  durably checkpointed *without* becoming a visible commit.
- 🔗 **[Real Git interop](doc/git-compatibility.md)** — import, clone, fetch,
  pull, and push against native Git remotes; compatibility is verified as
  observable behavior, and unsupported options fail *before* touching state.
- 📦 **[Bounded on big repos](doc/performance.md)** — streamed pack import holds
  memory nearly flat: the full Linux-kernel pack (≈6.6 GiB, 9.4 M delta objects)
  resolves under a fixed frontier at tens of MiB of RSS.
- ✂️ **[Content-addressed storage](doc/storage.md)** — text uses bounded line
  deltas, binary uses content-defined chunks; physical representation never
  changes file identity.
- 🌐 **[JVM and the browser](doc/clojurescript.md)** — repository, filesystem,
  query, diff, and the Git object/pack/protocol layers run on both Clojure and
  Node ClojureScript.

Part of the [replikativ](https://github.com/replikativ) ecosystem for
decentralized, queryable data.

## Installation

```clojure
org.replikativ/geschichte {:mvn/version "VERSION"}   ; see the Clojars badge
```

From a local checkout, for development:

```clojure
org.replikativ/geschichte {:local/root "../geschichte"}
```

## Five-minute Clojure example

```clojure
(require '[datahike.api :as d]
         '[geschichte.repo :as repo]
         '[geschichte.query :as query])

(def cfg {:store {:backend :memory :id (random-uuid)}
          :schema-flexibility :write
          :keep-history? true
          :commit-graph? true})

(d/create-database cfg)
(def conn (d/connect cfg))

(repo/init! conn {:name "demo"})
(repo/write! conn "README.md" (.getBytes "hello\n" "UTF-8"))
(repo/stage-all! conn)
(def initial (repo/commit! conn {:message "initial" :author "Ada"}))

(repo/status conn)                 ; Git-shaped status, as data
(query/commits @conn)              ; commits as ordinary values
(d/q '[:find ?path
       :where [?file :geschichte.work/path ?path]]
     @conn)                        ; …or just ask Datalog
```

See [Getting started](doc/getting-started.md) and the
[Clojure API guide](doc/clojure-api.md).

## Command line

`ges` exposes Git-compatible commands at the top level and keeps
geschichte-specific inspection under its own namespaces:

```sh
ges init
ges status --short
ges add -A
ges commit -m initial
ges log --oneline
ges db query '[:find ?path :where [?e :geschichte.work/path ?path]]'
```

From a source checkout, replace `ges` with `clojure -M:cli --`
(e.g. `clojure -M:cli -- status --short`).

Tagged releases ship native `ges` archives for Linux amd64, macOS arm64, and
macOS amd64. See the [CLI guide](doc/cli.md).

## Architecture

```
src/geschichte/
  repo.cljc            public repository API — init/write/stage/commit/status/log
  workspace.cljc       isolated Datahike worktree + index + ref catalog
  fs.cljc              versioned filesystem over the repository
  content.cljc         content-addressed blobs; logical IDs, physical is a choice
  chunk.cljc           content-defined chunking for binary content
  diff.cljc            line-vector Myers/bisect over text
  merge/               three-way merge
  query.cljc           Datalog-shaped views (commits, refs, paths)
  schema.cljc          the Datahike schema the repository lives in
  git/                 Git interop — object, pack, pack-index, revision,
                       protocol_v2, receive_pack, http, ssh, transport,
                       command, compatibility
  cli.clj              the `ges` entrypoint (native-image target)
```

Deeper: [Architecture](doc/architecture.md) (the two branch systems),
[Storage](doc/storage.md), and the
[Git compatibility contract](doc/git-compatibility.md).

## Documentation

- [Getting started](doc/getting-started.md)
- [Clojure API](doc/clojure-api.md)
- [Querying the repository](doc/querying.md)
- [ClojureScript](doc/clojurescript.md)
- [CLI](doc/cli.md)
- [Workspaces and publication](doc/workspaces.md)
- [Storage, deltas, and large files](doc/storage.md)
- [Muschel and dvergr integration](doc/integration.md)
- [Architecture](doc/architecture.md)
- [Git compatibility matrix](doc/git-compatibility.md)
- [Performance](doc/performance.md)
- [Native image](doc/native-image.md)
- [Prior art and positioning](doc/prior-art.md)
- [Building and releasing](doc/releasing.md)

## Development

```sh
clojure -M:test
clojure -M:format
clojure -M:cljs-test && node target/node-test.js
clojure -T:build jar
clojure -T:build native
clojure -T:build native-smoke
```

Babashka tasks remain as short aliases for these Clojure commands.

## License

geschichte is licensed under the [Apache License 2.0](LICENSE).
