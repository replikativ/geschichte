# `ges` command line

The JVM entrypoint is `geschichte.cli`; native releases package it as `ges`.

```sh
clojure -M:cli -- --help
clojure -M:cli -- status --help
ges status --help
```

Git-compatible commands are top-level. `ges git …` remains an alias for tools
that insist on a Git-shaped wrapper. Per-command help validates the same option
contract as execution.

## Physical projections

`ges init` creates `.geschichte/repo.edn`, a locator for the Datahike store and
workspace branch. Commands discover it from the current directory or parents.
Before worktree-sensitive operations, changed files are streamed into Geschichte;
checkout/reset stream the selected Geschichte tree back to the projection.

```sh
ges init
ges status --short
ges add -A
ges commit -m initial
ges worktree add ../agent main
ges -C ../agent workspace publish
ges workspace advance
```

## Native Git interoperability

```sh
ges import-git --force /path/to/worktree
ges clone https://github.com/replikativ/konserve.git konserve
ges clone --no-checkout https://github.com/torvalds/linux.git linux
ges fetch origin
ges pull --ff-only
ges push origin main
```

Local import does not invoke Git. It understands normal, bare, and linked
worktrees, loose/packed objects and refs, alternates, and SHA-1/SHA-256 formats.
Unsupported shallow/partial, reftable, replace/graft, or gitlink cases fail
explicitly.

`clone -n`/`clone --no-checkout` receives, validates, indexes, and publishes the
repository without materializing `HEAD`. This is useful for large repositories,
bare-style service use, and separating ingestion diagnostics from worktree I/O.

Set `-c geschichte.profile=true` before the command to report receive, checksum,
object discovery, delta resolution, index publication, and progress phases:

```sh
ges -c geschichte.profile=true clone --no-checkout URL DIRECTORY
```

Large-pack experiments may override bounded scanner controls through the same
Git-style `-c` mechanism: `geschichte.git.primitive-index-threshold`,
`geschichte.git.delta-parallelism`, `geschichte.git.delta-frontier-bytes`,
`geschichte.git.max-pack-index-memory-bytes`, `geschichte.git.pack-chunk-size`,
and `geschichte.git.delta-spill-directory`. Numeric values are bytes/counts and
are rejected before repository creation when malformed. The primitive bounded
scanner is the default; raising `primitive-index-threshold` selects the older
map decoder only for differential diagnostics.

`ges db query` exposes repository Datalog without colliding with Git command
names. See `ges db --help` for the administrative surface and
[the compatibility matrix](git-compatibility.md) for precise limits.
