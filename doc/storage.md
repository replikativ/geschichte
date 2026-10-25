# Storage, deltas, and large files

Paths and commits reference logical content IDs. Physical representation is an
implementation choice and never changes file identity.

```text
path/tree entry → logical content ID → full | line delta | chunk manifest
                                      → Datahike store ref
                                      → Konserve blob value
```

## Text

Valid UTF-8 text without NUL bytes is eligible for Myers line deltas. Delta chains
are bounded, so reconstruction cost cannot grow without limit. Unified Git-style
diff rendering uses the same portable line semantics on JVM and CLJS.

## Binary and large files

Binary data is byte-exact. Content-defined chunking finds stable boundaries, so
inserting bytes near the beginning of a large file does not shift every subsequent
chunk as fixed-size chunking would. Identical ranges reuse chunk IDs and store
payloads. Already-compressed formats may still change globally and therefore
deduplicate poorly; Geschichte does not run expensive format-specific transforms.

Payload writes are guarded until the store-ref transaction succeeds. A crash may
leave an unreferenced blob eligible for GC, but not a published dangling reference.

Replication must copy store-ref targets as well as Datahike indices. This is the
remaining integration requirement for complete large-file transport through
Datahike/Konserve synchronization.

## Reading byte ranges

`geschichte.content/consume-range-by-id!` accepts a logical content ID, offset,
length and consumer. It captures one immutable database snapshot and validates
the complete chunk manifest before fetching only the overlapping payloads. Each
selected payload is hash- and size-checked; a sparse read does not recompute the
whole logical file hash. Offsets and lengths must be nonnegative integers within
the file, and safe integers in ClojureScript. Empty ranges validate metadata
without loading payloads.

The consumer receives bounded byte slices in order. Returning
`content/stop-consumption` prevents later payload reads. On ClojureScript,
`:async-consumer? true` awaits each partial-CPS consumer before proceeding.
Full or delta representations reconstruct the complete value before slicing;
historical or custom-threshold values can therefore require more memory. The
caller remains responsible for authorization, operation lifetime and retaining
the referenced content against garbage collection.
