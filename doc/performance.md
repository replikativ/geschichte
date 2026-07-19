# Performance

Run the reproducible JVM comparison with:

```sh
clojure -M:bench
```

The synthetic input is about 239 KB: 5,000 lines with 50 distributed
replacements. On the development machine (GraalVM JDK 25), representative warm
medians were:

| Operation | Median |
|---|---:|
| Geschichte line-vector Myers kernel | 20–28 ms |
| Geschichte unified renderer | 3–5 ms |
| Geschichte text-to-unified end to end | 13–21 ms |
| Native `git diff --no-index` end to end | 13–25 ms |

The first renderer took roughly 49 ms because every hunk rescanned its entire
prefix. Prefix consumption tables reduced that component to about 4 ms. The
portable line scanner then proved to be the remaining JVM bottleneck: using the
JDK's optimized string splitter while retaining the final-newline field reduced
end-to-end time from 40–48 ms to 13–21 ms. On this input Geschichte is now in
the same range as a complete native Git subprocess. JVM scheduling and GC make
the small sample noisy, so these figures are a performance checkpoint rather
than a general claim that the algorithms have equal throughput.

Further optimization should preserve `geschichte.diff` as the portable
semantics/reference implementation while allowing platform kernels:

- JVM: byte-oriented scanning and a primitive-array Myers/bisect kernel only if
  larger corpora show the JDK splitter or retained-frontier algorithm dominating;
- JavaScript: typed-array CLJS first, then WASM only if browser benchmarks justify
  the boundary cost;
- all kernels: differential fixtures against native Git and the portable result.

The current result is sufficient to continue in Clojure at the API/model layer;
Java or WASM is no longer justified by this input alone. Pack delta selection
still needs a separate dataset and benchmark because line-review deltas and
byte-level wire deltas optimize different things.

## Large Git pack checkpoint

The native CLI was exercised against the canonical Linux kernel repository on
2026-07-19. The GitHub response contained 9,476,437 delta objects in a roughly
6.6 GiB pack. A streaming receive kept RSS near 40–50 MiB. Compact object
discovery and delta resolution peaked at 1,830,656 KiB RSS under the 3 GiB native
heap ceiling.

The first breadth-oriented resolver retained the unresolved dependency width and
grew a reusable spill file beyond 7 GiB. The depth-first resolver retained only
the active chain and bounded siblings: this kernel run created no spill file at
all with a 256 MiB in-memory frontier. It resolved all deltas and published the
16 compact store-ref index shards successfully.

The memory result is the durable win; timing is still being optimized. The
current bounded primitive resolver (default since 87c92f2) runs the kernel
import with no memory, spill, or disk alarm at any point. On a representative
recent run the phases were roughly: receive ~20m33s, object discovery ~10m32s,
and delta resolution proceeding steadily through the 9.4 M objects under the
fixed frontier — while an ordinary default-sized clone finishes end to end in
~4.6 s at ~224% CPU and ~230 MiB peak RSS.

The progress curve shows independent root fronts help, but dense subtrees still
cause load imbalance — one active node's chain starves the others. **Dynamic
subtree work-stealing is the next performance step.** The bound to preserve
throughout is memory: correctness and flat RSS first, wall-clock second.

Next large-pack work, in order:

- dynamic work-stealing across independent bounded DFS fronts so dense subtrees
  don't serialize on one core, retaining the global frontier cap;
- profile and optimize `inflate-at` and Git COPY/INSERT byte application,
  reaching for a small Java kernel only where measurement justifies it;
- report resolved-object progress periodically on large packs;
- keep `clone --no-checkout` as the ingestion boundary so checkout failures can
  be diagnosed without discarding a successfully published pack.

## Node CLJS checkpoint

`clojure -M:cljs-bench` and `node target/node-bench.js` compile with advanced
optimizations and exercise the same synthetic 239 KB input. The initial Node results were:

| Operation | Node CLJS median |
|---|---:|
| Text-to-unified diff | 19.5 ms |
| Native Node SHA-1 of 239 KB Git blob | 0.53 ms |
| Encode and decode 1,000 pkt-lines | 5.4 ms |

The diff result is within the observed JVM 13–21 ms range. Node hashing delegates
to native `crypto`, while buffers remain `Uint8Array` throughout the Git codecs.
