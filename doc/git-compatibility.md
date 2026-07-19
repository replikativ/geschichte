# Git compatibility contract

Geschichte treats compatibility as observable behavior, not merely accepting a
subcommand name. Unsupported options must fail before state changes. This file is
the implementation manifest for scenario replay and future native-Git differential
tests.

## Current command surface

| Area | Commands | Current faithful subset |
|---|---|---|
| Repository | `init`, `clone` | Muschel virtual mounts and standalone `.geschichte/repo.edn` physical projections; existing-file import, cwd discovery, origin/branch selection, `-n`/`--no-checkout`, HTTP/SSH transport, and direct local path/`file:` clone; no Geschichte bare projection |
| Native import | `ges import-git` | normal, bare, and linked-worktree discovery; loose/packed SHA-1 and SHA-256 objects, packed/loose refs, symbolic HEAD, alternates, useful config, safe in-place opt-in |
| Inspect | `status`, `diff`, `log`, `show`, `ls-files`, `rev-parse`, `merge-base` | common agent forms, short/porcelain-v1 status, two/three-dot revision ranges, history path filtering, name/stat output, formatted log, commit patches, `REV:path`, and ancestry exit-status checks |
| Stage/commit | `add`, `commit`, `rm`, `mv`, `restore`, `reset`, `clean` | three-tree local workflow, ignores/force-add, tracked `commit -a/-am`, path restore/reset, safe clean force policy |
| Refs | `branch`, `switch`, `checkout`, `tag`, `merge` | branch start points, lightweight tags, fast-forward and clean two-parent merges; conflicts are reported without unsafe partial mutation |
| Configuration | `config`, `remote` | local values/remotes persist in Datahike; host-global values are layered above them |
| Network | `fetch`, `pull`, `push` | one named remote, all advertised refs, fast-forward-only pull, one branch refspec; transport I/O is injected by the host |
| Workspaces | `worktree list/add/remove/prune` | physical JVM projections and Muschel mounts; named refs, `-b`/`-B`, conservative dirty removal, force removal, stale pruning, and multiple isolated checkouts of the same logical ref |

Global `--no-pager`, `--paginate`, and repeated `-C` are parsed before repository
discovery. Git commands are top-level in `ges`; `ges git ...` is a transition
alias. The native CLI supplies smart HTTP/SSH operations. An embedded sandbox
must inject a permitted transport adapter; the shared command engine never
silently escapes the host's process/network policy.

## Revision language

Implemented:

- `HEAD`, full logical refs, local branch/tag shorthand;
- materialized imported remote refs such as `origin/main`;
- unique logical commit UUID prefixes;
- repeated first/nth parent suffixes: `HEAD~3`, `merge^2`, `HEAD^^`.
- two-dot reachability and diff ranges: `A..B`;
- three-dot symmetric history and merge-base diff ranges: `A...B`;
- omitted range endpoints default to `HEAD`.

Next: multiple positive/negative revision operands such as `^REV`, reflog
selectors, peel/type suffixes, chronological/topological ordering modes, and
path ambiguity diagnostics matching Git.

## Deliberately incomplete behavior

- Standalone clone creates storage and a physical projection, but remote
  differential scenarios still need deeper fault injection. A newly created
  failed clone is removed atomically; `--no-checkout` preserves a successfully
  published repository without materializing its worktree.
- Local import deliberately rejects shallow/partial repositories, reftable,
  grafts, replace refs, and submodule gitlinks. Packfiles are streamed into
  immutable 4 MiB store-ref chunks; compact primitive indices and a bounded
  depth-first delta frontier avoid retaining the complete pack or unresolved
  object width in memory.
  REF_DELTA bases outside an incoming thin pack are resolved through the
  existing exact-object store, so incremental fetches use the same bounded
  primitive scanner as full clones.
- Pull accepts only explicit fast-forward-only behavior at the command layer.
- Merge does not yet install Git-compatible unmerged index stages or conflict
  markers; it reports conflicts and leaves the worktree untouched.
- No annotated/signed tags, rename/copy detection, binary patches, submodules,
  linked-Git administrative files, hooks, signing, attributes, filters, or sparse
  checkout. Detached worktrees are rejected before mutation.
- Reflog, archive, bundle, and parts of the low-level object/index plumbing remain
  incomplete.
- The outbound pack writer is valid but undeltified; imported packs support Git
  COPY/INSERT deltas.

## Systematic deepening order

1. Expand the differential scenario harness beyond revision selection to
   normalized status, index, refs, merges, and worktree state.
2. Host-level clone and fuller fetch/pull/push refspec/upstream behavior.
   Clone must create storage and a host projection, so it intentionally does
   not masquerade as an ordinary command against an already-open connection.
3. Git-compatible conflict index stages, conflict-marker materialization, abort
   flows, and then cherry-pick/rebase.
4. Stash and reflog/history recovery.
5. Plumbing commands only when observed agent/IDE usage requires them.

Every added option needs at least one positive behavior test and one test showing
that the closest unsupported spelling is rejected without changing repository
state.
