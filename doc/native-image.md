# Native CLI size and behavior

Native-image optimization is gated by an end-to-end executable workflow, not
only `--help`. The smoke test covers physical init/discovery, status, add,
commit, history, branch checkout materialization, nested `-C`, and failure
atomicity for an unsupported mutating option.

## 2026-07-17 baseline and first reduction

The previous stripped Linux executable was 141,429,072 bytes (about 135 MiB):

| ELF area | Bytes |
|---|---:|
| `.text` | 52,689,753 |
| `.svm_heap` | 56,360,960 |
| `.rela.dyn` | 31,972,224 |

The old `clj.native-image` wrapper also placed its own Maven, AWS SDK, JGit,
JNA, ClojureScript, and Closure Compiler dependencies on GraalVM's analysis
classpath. Geschichte now AOT-compiles with a build-only namespace and passes a
separately resolved runtime classpath to `native-image`. JVM-only native aliases
exclude Datahike/Konserve's CLJS backend dependencies. GraalVM size optimization
is enabled with `-Os`.

The resulting executable is 106,957,136 bytes (about 102 MiB):

| ELF area | Bytes | Change |
|---|---:|---:|
| `.text` | 21,946,137 | -58% |
| `.svm_heap` | 52,232,192 | -7% |
| `.rela.dyn` | 32,419,920 | +1% |

On the measurement host, `ges --help` starts in about 40 ms with 61 MiB peak
RSS and a clean physical `status` in about 60 ms with 73 MiB peak RSS.

## Remaining size frontier

GraalVM reports roughly 21.75 MiB of code, 52.23 MiB of image heap, and 39.15
MiB of other data. The largest application origins include Clojure, Datahike,
Datahike query execution, Konserve/tiered storage, Jackson, Java HTTP, and Malli.
The image contains about 24,500 reachable types and 99,200 reachable methods.

The next useful experiments therefore belong mostly upstream in Datahike:

1. expose a minimal, manually defined runtime API instead of eagerly loading the
   generated full API/specification surface;
2. move Malli/specification and optional HTTP/JSON dependencies out of minimal
   embedded profiles;
3. replace open `requiring-resolve` paths with explicit native registries for
   query functions, transaction functions, stores, indices, and serializers;
4. split standing replication/network service code into an optional `gesd`
   process if delayed loading cannot keep it out of the one-shot CLI;
5. compare the full Datalog command with a VCS-only image before deciding
   whether two distributed binaries are worth the interface cost.

Compression can be evaluated for release artifacts afterward, but it does not
reduce reachable code, runtime RSS, or image-heap initialization.
