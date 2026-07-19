# Building and releasing

tools.build is authoritative; `bb.edn` contains convenience aliases only.

```sh
clojure -M:format
clojure -M:test
clojure -M:cljs-test && node target/node-test.js
clojure -T:build clean
clojure -T:build jar
clojure -T:build install
```

Versions follow `0.1.<git revision count>` unless `GESCHICHTE_VERSION` is set.
On CircleCI, a leading `v` is removed from `CIRCLE_TAG` and that value becomes
the artifact version.

## CircleCI

CircleCI runs format, JVM tests, Node CLJS tests, and jar construction on every
branch. Publishing is deliberately explicit: a `v*` tag uses the
`clojars-deploy` context to publish the jar and the `github-token` context to
create the matching GitHub release with the jar attached.

Required context variables:

- `CLOJARS_USERNAME`
- `CLOJARS_PASSWORD`
- `GITHUB_TOKEN`

## Native images

A pushed `v*` tag builds and smoke-tests Linux amd64, macOS arm64, and macOS
amd64 executables. Archives and SHA-256 files are attached to the existing GitHub
release. Pull requests exercise the same workflow before release.

Local native build:

```sh
clojure -T:build native
clojure -T:build native-smoke
```

GraalVM 25 and `native-image` on `PATH` are required. See
[native-image.md](native-image.md) for size and startup measurements.
