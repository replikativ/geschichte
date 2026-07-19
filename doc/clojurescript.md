# ClojureScript

Geschichte shares repository, filesystem, query, content, diff, Git object,
pack, protocol, revision, and merge semantics between Clojure and ClojureScript.
Node uses `Uint8Array` and its native crypto implementation rather than converting
binary data to persistent Clojure vectors.

Compile and run the Node gate with:

```sh
clojure -M:cljs-test
node target/node-test.js
```

Datahike database creation, connection, transactions, versioning, and store-ref
operations are asynchronous on Node/browser platforms. Geschichte exposes these
effects through partial-CPS. JVM callers retain direct synchronous values.

Host facilities are separate capabilities:

- repository and query APIs do not require Muschel;
- browser filesystem/CLI behavior requires a host filesystem adapter;
- JVM smart HTTP and SSH process transports are not implicitly available in a
  browser sandbox;
- Node hosts may inject their own permitted transport implementation.

The Git-shaped interpretation and Datalog query views are therefore usable for a
browser UI even when clone/push and host filesystem projection remain server-side.
