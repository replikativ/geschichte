# Per-operation Git import budgets

JVM Git imports optionally accept one operation-local `:import-budget`. The
host creates it after admitting the operation, passes it through discovery,
fetch and checkout, and retains it for any later lazy object reconstruction
that is part of the same operation. It is never installed in a shared cache.

```clojure
(require '[geschichte.git.budget :as budget]
         '[geschichte.git.http :as http]
         '[geschichte.git.store :as objects])

(let [operation (budget/create
                 {:timeout-ms 60000
                  :cancelled? (fn [] @cancelled?)
                  :limits {:response-bytes (* 64 1024 1024)
                           :pack-bytes (* 64 1024 1024)
                           :objects 100000
                           :index-bytes (* 64 1024 1024)
                           :object-bytes (* 16 1024 1024)
                           :inflated-bytes (* 256 1024 1024)
                           :delta-result-bytes (* 16 1024 1024)
                           :reconstructed-bytes (* 256 1024 1024)
                           :delta-depth 64
                           :commit-visits 10000
                           :tree-entries 100000
                           :tree-depth 64
                           :checkout-files 10000
                           :checkout-bytes (* 128 1024 1024)}})]
  ;; private-conn must be exclusively owned, initialized and unpublished.
  (http/clone! private-conn "origin" "https://github.com/owner/repository.git"
               {:import-budget operation :http-body-idle-timeout-ms 15000})
  ;; Additional reads in this operation use that same handle.
  (objects/read-object private-conn oid {:import-budget operation})
  (budget/snapshot operation))
```

These numbers illustrate a policy; they are not universal safe defaults.
Omitted dimensions are unbounded apart from JVM representation restrictions
and existing parser limits. No budget preserves the existing trusted-caller
contract. An empty budget only supplies cooperative checkpoints and counters.
Unknown policy options/metrics, fractional/negative amounts and integers beyond
the signed-long range are rejected. Zero is a valid resource limit.

## Counter meanings

| Cumulative metric | Unit and charge point |
| --- | --- |
| `:response-bytes` | Bytes actually read across every HTTP response, including advertisement, refs, progress, error bodies and pack framing. A nested wrapper using the same handle counts once. |
| `:pack-bytes` | Bytes admitted to the pack sink before copying or writing chunks. |
| `:objects` | Declared pack entries before either scanner allocates its index. |
| `:inflated-bytes` | Actual zlib output, before hashing or retaining each chunk. Repeated scans/reconstruction count again. |
| `:reconstructed-bytes` | Declared delta output reserved before allocation. A subsequent malformed delta may fail without producing all reserved bytes. |
| `:commit-visits` | History/ancestry traversal work. Checkout counts each pending visit, including repeated edges. |
| `:tree-entries` | Every tree entry visited, including repeated visits to a shared tree under different paths. |
| `:checkout-files` | Each emitted file path, counted before reading its blob. |
| `:checkout-bytes` | Bytes emitted per file path, including repeated references to the same blob. |

| Per-value bound | Meaning |
| --- | --- |
| `:index-bytes` | Compact index workspace estimate before index allocation; currently 58 bytes per pack entry. This is not a bound on total process heap. |
| `:object-bytes` | Declared inflated object/delta-instruction size, and resolved output size. |
| `:delta-result-bytes` | Declared delta target size before output allocation. |
| `:delta-depth` | Delta edges through internal and external pack bases. A whole object has depth zero. |
| `:tree-depth` | Directory depth; the selected root tree has depth zero. |

Limits apply to both scanner implementations. Budgeted reads bypass shared
decoded payload cache hits, which cannot prove the reconstruction depth/work
already hidden by another caller. The underlying byte/chunk cache remains
usable. Actual decoding is charged again when it is repeated. Budget counters
are atomic across parallel decoder workers; the first refusal is terminal for
the operation. Decoder workers settle before their shared spill storage closes.

## Refusals and transport ownership

Resource refusal is `ExceptionInfo` with
`{:type :geschichte.git.budget/exceeded :metric ... :limit ... :used ...
:requested ...}`. A per-value bound reports `:used 0`. Cooperative cancellation
uses `:geschichte.git.budget/cancelled`; elapsed timeout uses
`:geschichte.git.budget/deadline-exceeded`. Malformed Git data remains a parser
error. `budget/snapshot` can be read after refusal without clearing it.

Checks occur before and after stream reads, during inflation/reconstruction and
traversal, and before publication transactions. HTTP requests carry remaining
`:timeout-ms` to the injected sender. The default Java sender applies that
request timeout and checks cancellation while awaiting response headers. It
cancels its native request on refusal. An operation-owned monitor closes the
native response body on operation deadline/cancellation or optional positive
`:http-body-idle-timeout-ms`. Idle timeout measures a pending blocking read,
not CPU/storage work between reads. Body closure joins that monitor before the
operation returns. Real loopback tests hold the server stalled until the client
has exited for header timeout/cancellation and body idle/deadline/cancellation.

The host must ensure a custom sender honors its timeout, physical cancellation,
and destination/redirect policy. It receives `:timeout-ms`, `:import-budget`
and `:http-body-idle-timeout-ms` in its request map. The default Java client does
not follow redirects. Response bodies close on success and refusal,
including invalid status/content type. A read may receive one bounded buffer
past the remaining byte limit before refusing; those bytes are not published.

The close/cancel behavior above is specific to the default Java HTTP transport.
The remaining checkpoints do **not** guarantee interruption of an arbitrary
blocked custom stream, sender, storage backend or callback. The host owns
physical cancellation and teardown where those guarantees are needed.
Budgeted SSH/process transport explicitly refuses before launching a process;
it currently lacks bounded process I/O/teardown. Import budgets do not bound Git
push/export or divergent merge preparation; those paths explicitly refuse a
budget rather than silently dropping it. A budgeted pull also refuses local
Git projection when the current logical commit has no existing Git object ID.
Fast-forward synchronization of imported Git histories is supported.

## Private preparation and publication

The host must limit concurrent imports before allocation and hold exclusive
ownership of the private preparation connection. A refused pack ingestion
publishes neither its pack membership nor remote-tracking refs; already written
unreferenced blobs may remain for garbage collection.

A successful ingestion can publish object membership **in the private store**
before a later checkout refuses. Checkout materialization uses multiple
transactions and may leave partial private worktree preparation. This API does
not promise all-operation rollback. No such preparation should be exposed as a
successful live Memory/sync. The host owns cleanup and must separately validate
authority, source/target versions and the completed result before guarded
publication to a live repository. Direct pull into a concurrently edited live
Memory remains unsupported.
