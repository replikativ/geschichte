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

