# Bounded native merge planning

`geschichte.merge/plan` is a JVM API. Its existing calls retain their behavior.
Pass `:merge-budget` to opt into the complete finite resource profile:

```clojure
(require '[geschichte.budget :as budget]
         '[geschichte.merge :as merge])

(let [operation (budget/create (merge/recommended-budget-policy 30000))]
  (merge/plan private-connection ours-id theirs-id
              {:merge-budget operation :merge-content? true}))
```

`merge/budget-profile` returns the versioned compatibility marker and required
metric names. A host must verify this marker before assuming an older library
understands `:merge-budget`. The recommended policy is a baseline, not hidden
fallback limits: callers can change its finite values. Every required dimension
and a positive operation deadline must be present before the planner reads its
source. Zero limits are valid policies and refuse work when it is attempted.

For fetch followed by merge, combine the import and merge limit maps **before**
creating one operation. The Git compatibility wrapper and portable budget kernel
share the same handle and counters. Passing the existing handle to the planner
never restarts its deadline or replenishes its allowance. A first refusal is
terminal. The handle contains native mutable control state and a borrowed
cancellation predicate; do not serialize it, store it in Datahike, or copy it into
a simulated world. `budget/snapshot` returns ordinary policy/counter data.

## Snapshot and publication contract

A bounded plan captures one immutable catalog database. Both native ancestry
walks, commit checkpoint selection, all three tree projections and existing
content metadata use that snapshot. Historical stage sizes must agree with that
snapshot's content metadata. Payloads remain content addressed and are hash
checked when read. The planner does not eagerly read unchanged structural blobs;
logical tree-byte accounting is distinct from physical payload accounting.

The native BFS and existing diff3/Myers strategy remain in use. The new mode does
not replace an expensive diff with a different algorithm. Exhausting Myers'
legacy fixed trace capacity is a typed refusal under a budget. Existing calls
without a budget keep their legacy fallback behavior.

Planning does not advance a ref, stage a merge or change the worktree. The
existing content resolver **can write immutable merged content and its metadata**
to the caller-owned private connection. Those writes may remain if another path
later conflicts or exceeds a limit. The host owns this private preparation and
its cleanup. Only an explicit guarded publication step may affect the canonical
repository. A clean plan is not itself an approval or publication permission.

## Units

All counters are cumulative over one operation. Repeated reads and visits count
again, including the same ancestor reached by the two source walks and the same
file present in all three trees. Limits are checked before retaining projected
entries, output vectors, reconstructed buffers or trace copies.

| Metric | Counted resource |
| --- | --- |
| `:commit-visits` | Native outgoing-adjacency lookups, including zero-edge roots |
| `:graph-edges` | Parent neighbors consumed by the native walks |
| `:tree-entries` | Historical stage path entries inspected across the three trees |
| `:tree-path-bytes` | UTF-8 bytes of those visited paths |
| `:tree-bytes` | Declared logical bytes of present input entries, checked against content metadata |
| `:result-tree-bytes` | Logical bytes projected into the candidate before structural conflict pruning |
| `:content-visits` | Recursive content reconstruction attempts |
| `:content-parts` | Chunk metadata entries visited |
| `:content-read-bytes` | Physical payload bytes admitted from a backend callback |
| `:content-decoded-bytes` | Reconstructed logical bytes, including repeated bases/chunks |
| `:text-lines` | Input and expanded/intermediate line-vector elements |
| `:text-work` | Checked character/line/algorithm work, including conservative map-copy comparison reservations |
| `:text-trace-cells` | Cells retained in Myers frontier copies |
| `:delta-forms` | Generated-delta structural forms and atomic/string tokens |
| `:merge-output-bytes` | UTF-8 bytes of newly assembled merge text, including conflict display text |

Per-value bounds are `:object-bytes`, `:path-bytes`, `:content-depth`,
`:delta-edn-depth`, `:delta-token-chars` and `:text-frontier-cells`. Input and
candidate tree byte budgets include unchanged files and structural-only plans;
`:merge-output-bytes` specifically measures assembled text, not the whole tree.

Generated line deltas receive a lexical preflight over every character before
EDN parsing, followed by strict generated-schema/range checks. Dispatch, tagged,
discard, list, comment and character forms are unsupported. Expansion includes
base lines and inserted strings in the byte/line budget before concatenation.
This is a guard for Geschichte's generated format, not a general cancellable EDN
parser. The bounded EDN reader and native codec/hash calls are finite operations
between cooperative checkpoints.

## Backend ownership and cancellation

JVM binary streams are consumed inside Konserve's locked callback using a fixed
8 KiB scratch buffer. The callback does not close a borrowed stream. Its owning
backend retains lock/close responsibility. Byte-buffer backends are checked at
handoff; the backend may already have allocated their returned buffer.

Budgeted **Node stream** reads are explicitly unsupported in this first native
planner delivery. Konserve's Node filestore lends a ReadStream over an fd it
owns; destroying that stream can close the borrowed fd twice, while waiting for
its close can deadlock ownership. The bounded callback therefore returns a typed
unsupported refusal before consuming the stream, adding data listeners or
calling destroy/pause/close. Konserve completes its ordinary owner cleanup.
Unbudgeted Node reads and budgeted byte-buffer backends retain their behavior.
Portable text, budget and generated-delta guards run in both JVM and Node tests.

Deadline/cancellation checks are cooperative. This API does not promise to
interrupt arbitrary blocked storage implementations or custom streams. The host
must retain operation ownership and admission slots until physical exit. Native
HTTP fetch has its separately documented owned-body watchdog; it does not make
an arbitrary storage read preemptible.

Typed errors use `geschichte.budget` (or the existing Git error namespace when
using a Git-created operation): `invalid-policy`, `exceeded`,
`deadline-exceeded`, `cancelled`, `unsupported`. These errors escape the content
resolver; they are not converted into ordinary merge conflicts. Malformed projected tree
metadata is a source error rather than a reason to refresh or substitute a newer
commit. No force publication or default ref deletion is introduced here.
