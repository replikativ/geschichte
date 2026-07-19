# Muschel and dvergr integration

Geschichte can be the filesystem rather than a repository hidden behind one.

```text
Datahike/Konserve repository
          ↓
geschichte.fs
          ↓
muschel.fs.geschichte / MountFS
          ├── shell builtins
          ├── integrated `git`
          ├── SCI slurp/spit/require
          └── structured agent tools and app serving
```

Muschel's Geschichte adapter deliberately returns no physical path. Its Git
builtin discovers mounted repositories and executes Geschichte's shared command
engine directly. A dvergr room owns a persistent Geschichte store; a room fork
owns a structurally shared Datahike workspace branch. Merge publishes and advances
the parent, while discard deletes the child branch.

No `.git` directory or materialized checkout is required for normal sandbox work.
Native compilers and external processes are the exception: they need an explicit,
short-lived projection capability that materializes a snapshot, executes inside
an OS sandbox, hashes/imports changes, and deletes the projection. Without that
capability Muschel fails closed instead of falling back to the daemon's cwd.

Network Git is also host-injected. The standalone JVM CLI supplies HTTP/SSH
transports; embedded sandboxes must provide a transport constrained by their
domain, credential, and process policy.
