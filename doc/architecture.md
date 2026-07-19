# Architecture

## Versioning layers

Datahike and Geschichte deliberately expose two branch systems:

1. A **Datahike branch** is an isolated complete workspace used for agent forks,
   speculative execution, merge-down, and discard.
2. A **Geschichte ref** (`refs/heads/main`) is repository data inside a workspace.

Every filesystem or staging transaction creates a durable Datahike checkpoint.
A user-visible Geschichte commit selects the checkpoint immediately before its
commit-marker transaction. This gives autosaved work without placing every write
in user-visible history.

```
Datahike:   C0 -- write C1 -- stage C2 -- marker/ref C3 -- write C4
Geschichte:                         commit A -> snapshot C2
Git:                                 commit OID -> projection of A
```

The canonical Datahike `:db` branch is deliberately headless: it is the serialized
publication authority, not a checkout. Every physical or virtual checkout has a
private Datahike branch, even the initial directory. Thus several agents can check
out `refs/heads/main` simultaneously without Git's shared-worktree restriction.
They commit locally, explicitly publish a fast-forward into `:db`, and explicitly
advance clean peer workspaces from it. The local `.geschichte/workspaces.edn`
registry maps projection paths to branches using locked atomic replacement; it is
recoverable coordination metadata rather than replicated repository history.

## Payload ordering

File bytes are immutable values in the database's Konserve store. Geschichte:

1. computes `datahike.blob/blob-id`;
2. opens `datahike.gc-guard/with-unreferenced-writes`;
3. writes bytes with `k/bassoc`;
4. transacts a `:db.type/store-ref` datom;
5. closes the guard after the transaction publishes the new branch head.

A crash can therefore leave an orphan eligible for GC, never a published dangling
file reference.

Paths do not point directly at physical payloads. They point at logical content
entities identified by the hash of reconstructed bytes. A content entity records
`:full` or `:line-delta`, its store-ref payload, optional base-content ref, logical
size, and bounded delta depth. Consequently a snapshot, diff, merge, or Git export
observes the same content ID regardless of compaction decisions. Invalid UTF-8 and
NUL-bearing files remain byte-exact full payloads.

## Required Datahike extension

`:geschichte.commit/snapshot` is currently a UUID. Before history-window GC is
enabled it should become a generic Datahike commit root, tentatively
`:db.type/commit-ref`. GC and replication must follow such values to the stored
commit record, its indices, its payload store-refs, and retained parents.

## Host filesystem boundary

`geschichte.fs` exposes the Datahike working tree without depending on Muschel,
dvergr, or a physical checkout. Hosts adapt its byte-oriented operations to their
filesystem protocol. Directory nodes are workspace state: parent directories are
derived from file paths and explicit empty directories live in Datahike, but only
files enter Geschichte/Git commits.

```text
agent shell / SCI loader
          |
          v
 host FS adapter (for example muschel.fs/FS)
          |
          v
 geschichte.fs  -- byte I/O, stat/list, mkdir, rename, delete
          |
          +--> geschichte.repo -- worktree, index, refs, commit DAG
          |          |
          |          +--> Datahike versioning -- checkpoint, branch, merge, as-of
          |                         |
          +--> geschichte.content --+--> Konserve raw blobs/full + line deltas

Git HTTP/SSH <--> object/pack/protocol adapters <--> the same commit DAG
```

Native `.git` import enters at the exact-object side of this boundary. A JVM
adapter discovers normal/bare/linked Git directories, follows `commondir` and
object alternates, imports loose objects and complete packfiles, interprets refs
and config, and then invokes the same lazy checkout used after HTTP/SSH fetch.
The native Git index is intentionally not copied: Geschichte reconstructs its
own clean index from the selected commit, while an in-place import requires an
explicit overwrite opt-in. Exact Git objects remain available for round trips.

This is now the dvergr integration seam. Muschel's Geschichte adapter exposes the
repository as a virtual root, its integrated `git` uses the same command engine,
and dvergr routes shell builtins, SCI loading and file I/O, structured tools,
skills, and app serving through that root. The Yggdrasil adapter advertises
snapshot, branch, graph, merge, and commit capabilities without inventing a fake
physical path. External native processes remain an explicit projection boundary.

## CLJ/CLJS boundary

Portable semantics live in CLJC and operate on native platform buffers: JVM
`byte[]` and CLJS `Uint8Array`. The shared byte facade performs bulk slicing and
concatenation, while codec hot loops retain direct primitive array access through
reader conditionals. This avoids persistent-vector conversion and per-byte
protocol dispatch.

The first Node gate covers Myers/unified diff, canonical Git SHA-1/SHA-256 object
identity, binary tree parsing, pkt-line, protocol-v2 and receive-pack framing,
sideband reconstruction, commit ancestry, and three-tree merge planning under
advanced compilation. Node uses native `crypto`; JVM stream reads remain a thin
platform-only extension to the portable pkt-line codec.

Repository and storage mutation are the next portability boundary. JVM Datahike
calls are synchronous, while Node/browser create, connect, transact, branch, and
store-ref operations are asynchronous. Pure queries and merge/diff interpretation
remain synchronous. Geschichte will follow Datahike's platform-default `:sync?`
model rather than making pure algorithms asynchronous.

`geschichte.query` is the first database-backed portable slice. Node tests create
and connect asynchronously, install the full schema and transact repository data,
then query repository identity, refs, commits, worktree/index metadata, content
representation, and imported Git catalogs synchronously from `@conn`. The same
views accept historical values returned by `commit-as-db`, so a browser UI does
not need separate Git-shaped indexes to interpret a synchronized repository.

Effectful portable APIs use `partial-cps`, matching Yggdrasil and Muschel's async
interpreter branch. `geschichte.macros/async+sync` compiles one body to direct JVM
code by erasing `async`/`await`, or to an awaitable CPS value on CLJS. Datahike and
Konserve currently return core.async channels for asynchronous operations;
`geschichte.async/io-result` uses partial-cps's optional channel bridge so channels
remain an implementation boundary rather than Geschichte's public composition
model. JavaScript Promises have a similarly small CPS adapter.

This preserves the existing synchronous Clojure API and its fast path while letting
Geschichte operations compose directly inside the partial-cps Muschel interpreter.
Browser or Node shell I/O still requires Muschel's CLJS filesystem/host adapters;
repository use and synchronization do not depend on the shell being present.

## Git compatibility sequence

1. Exact blob/tree/commit/tag encoders and SHA-1/SHA-256 object IDs. **Started:**
   canonical encoders and IDs are implemented; object persistence/import is next.
2. Pack reader including REF_DELTA, OFS_DELTA, thin packs, and zlib streams.
   **Implemented:** strict envelope/checksum parsing, whole objects, both delta
   reference forms, COPY/INSERT reconstruction, caller-supplied thin bases, and
   reuse of persisted bases during Datahike import. Native Git differential tests
   have exercised packs dominated by both OFS_DELTA and REF_DELTA entries.
3. Initially undeltified valid pack writer. **Implemented:** pack-v2 object
   headers, zlib payloads, deterministic order, and SHA-1 trailer. Generated
   packs are accepted by native `git index-pack --stdin`.
4. Bundle import/export and native `git fsck` differential tests.
5. Smart HTTP capability discovery, protocol-v2 `ls-refs` and fetch. **Started:**
   strict pkt-line framing, capability advertisements, and transport-independent
   `ls-refs`/fetch request construction plus `ls-refs` response parsing are
   implemented. Fetch responses reconstruct sideband pack streams and progress.
   Native `git upload-pack` accepts the generated `ls-refs` and fetch requests.
   An end-to-end differential run fetched 57 objects (including 11 deltas),
   reconstructed the sideband pack, and persisted all objects into Datahike.
6. Receive-pack push, remote-tracking refs, and pull composition. **Started:**
   remote-tracking refs plus receive-pack advertisement, ref-update request, and
   report-status parsing are implemented. Push intentionally remains separate from
   protocol v2 because Git does not expose it as a v2 command.
   A native differential push accepted a Geschichte-written 89-object pack and
   ref update (`unpack ok`); the resulting bare repository passes `git fsck
   --strict` and resolves the expected HEAD.

Smart HTTP is a thin injectable transport: it constructs discovery and RPC
requests, validates status/media types, removes the service prelude, and delegates
all protocol bytes to the same fetch/push paths used by native-process differential
tests. Java's HTTP client is the default; tests substitute an in-memory sender.
Native `git http-backend` validation exercised discovery and fetch end to end,
importing 103 objects (36 REF_DELTA entries) and both HEAD/main refs.
The same backend accepted a Geschichte-projected commit and pack over smart HTTP,
returned `unpack ok`, updated main, and produced a repository passing strict fsck.

SSH uses a stateful process seam: stream the service advertisement through its
flush packet, write one protocol command on the same process, then consume the
response. Command construction is argv-based with an escaped remote repository
argument; the same seam launches local Git services for differential tests.
Local stateful validation fetched 127 objects (45 deltas). The receive-pack side
uses the same advertisement/request session and supports Geschichte push over SSH.

Pull compares exact imported/projected Git ancestry before touching the worktree.
It reports up-to-date and local-ahead states, materializes a true fast-forward, or
rejects divergence under `:ff-only`. With `:merge`, the remote tip is materialized
under a temporary logical ref and fed into Geschichte's two-parent merge planner.
7. Optimized pack delta selection sharing Geschichte's byte-match kernel.

Imported Git commits may remain object records until checked out. Materializing
every commit of a very large foreign history as a Datahike checkpoint is optional,
not a requirement for preserving or pushing its Git OID.

Fetched refs live in a separate Git ref catalog (`refs/remotes/...`) and point at
OIDs. On checkout, Geschichte parses the selected commit/tree graph, creates
lightweight logical metadata for its ancestors, writes only the selected tree into
the versioned filesystem, and pins that one Datahike checkpoint. Selecting an
ancestor later can fill its snapshot lazily without changing its Git mapping.
The native differential path currently fetches Geschichte's own four-commit Git
history (70 packed objects, including 18 deltas), imports it into a fresh Datahike
database, materializes HEAD's 28 files, and reproduces the four-message log with a
clean worktree.

The inverse path is implemented at the object-graph boundary: a Geschichte commit
and its reachable parents project to canonical blobs, trees, and commits; the graph
can be persisted as exact Git payloads keyed by OID, with commit-to-OID mappings.
Pack and wire adapters can therefore operate on Git objects without coupling their
identity to Geschichte's physical full/delta content representation.

## Merge boundary

Merge-base traversal operates on Geschichte's explicit parent graph. Three-tree
planning compares path metadata containing logical content IDs, not physical
payload IDs, and implements the standard unchanged-on-one-side resolution rules.
Conflicts retain base/ours/theirs values. Clean plans can replace the worktree and
index, record a pending second parent, and produce a two-parent commit. Persisted
conflict entities, textual conflict resolution, rename detection, and recursive
criss-cross merge bases are the next layer; the pure planner keeps those policies
testable and filesystem-independent.
