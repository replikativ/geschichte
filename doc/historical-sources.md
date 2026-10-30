# Historical source and lifetime contract

A logical content UUID identifies bytes, not a representation in the current
workspace catalog. Selected-tip publication can retain an ancestor's immutable
snapshot while omitting that ancestor's payload metadata from the current DB.

`repo/source-view` resolves one selected source to an immutable DB and a pure
tree. It is a short-lived reader, not a retention root. The caller keeps its
store and physical ancestry alive while using it. Tree entries contain only
logical content IDs, sizes, and modes; no live handles or provenance are stored.
`repo/read-entry` accepts a third source argument for historical entries. Its
two-argument form reads the current catalog. `repo/read-at` resolves the source
and path together. CLI diff/log/show retain separate before and after views.
Structural diff output avoids payload reads unless rename similarity or
whitespace filtering requests them; raw IDs, patches, and statistics need bytes.

Checkout, restore, and mixed/hard reset validate paths and dirty-state rules,
then restore missing selected bytes through the existing guarded content writer
before their public installation transaction. Logical content IDs remain the
same even when preparation chooses a full representation instead of a delta or
chunks. Merge uses the same helper. A later preparation/refusal can leave
immutable content in the private catalog; HEAD, worktree, and index installation
remains one transaction under the caller's serialized workspace ownership.
Soft reset does not prepare content. Selected historical publication/advance
reads representation closure from the selected snapshot and logical ancestry
from the captured catalog. Existing serialized target guards still apply.

## Datahike 0.8.1910 retention

Supported retention requires `:commit-graph? true`, `:keep-history? true`, retained
native branch/merge ancestry, and no history-pruning cutoff. The pinned default
`d/gc-storage` uses the beginning of time as its history cutoff. File-store
regressions remove the publishing branch, close all connections, reconnect,
collect with that default history cutoff and a zero sweep floor in the isolated
fixture, reconnect again, then reconstruct actual full/delta/chunk historical
bytes, delta-base content, and exact packed bytes through their index shards. These prove more than snapshot-record existence.

A logical `:geschichte.commit/snapshot` UUID is an ordinary attribute, not an
independent collector root. A restrictive `remove-before` can stop native-parent
traversal and remove that snapshot even while its logical commit row remains.
The regression characterizes this unsupported configuration explicitly. Do not
run such pruning when promising retained logical history without separately
establishing supported physical roots and their lifecycle. This change chooses
no new root integration or reclaim policy. Collection during active preparation,
shared/operator collectors, external roots, and process-death cleanup need the
host's ownership and collector policy; they are not proven by these tests.

Selective logical publication excludes private work/index/config/refs from the
current canonical catalog. Its native merge ancestry may retain private physical
history for snapshot lifetime. It does not promise selective physical retention.

## Semantic decisions left explicit

The existing merge-base policy minimizes combined shortest-parent distance,
then logical-ID order. A redundant shortcut edge can make it choose an ancestor
of another common ancestor. The minimal DAG regression records that behavior;
it is not Git's best-common-ancestor antichain. A future behavior change should
select the antichain under the graph budget and choose an explicit multiple-base
policy, for example refusal before implementing recursive virtual bases.

Authored logical parents are a set. Bounded authored Git projection sorts parent
OIDs for stable cross-peer identity, so it does not preserve semantic first-parent
order. Imported exact objects retain their original serialized order. Adding
ordered-parent metadata requires a versioned authored projection and must never
rewrite already acknowledged OIDs. These decisions are separate from historical
source repair.
