# Bounded incremental Git push

`geschichte.git.push` provides an opt-in JVM API for hosts that need finite
export work and an explicit publication decision. Existing Git export APIs are
unchanged. This API does not grant permissions, manage credentials, retry a
request, or persist a host's operation journal.

The host supplies a complete operation from `recommended-policy`, a selected
`refs/heads/...` ref, and its exact expected remote SHA-1 OID. Use the explicit
all-zero OID for a branch that must not exist. `prepare-http!` checks the
advertised ref before reading the source, then projects one immutable source
snapshot. It returns a bounded pack, logical commit/OID mappings, the source
checkpoint, and the exact old/new OIDs.

## Ownership and publication sequence

1. Capture the host's permissions, source connection and selected ref. Admit
   finite work with a deadline. The HTTP sender must enforce the host's network
   and credential policy.
2. Call `prepare-http!`. Review and bind the result to the captured source;
   never accept prepared pack bytes or mappings from an untrusted wire request.
3. In the source's owned serialized writer, apply `mapping-tx`. Its exact
   checkpoint guard refuses any intervening source change. Require a positive
   transaction acknowledgement before proceeding. These mappings establish Git
   identity, not remote presence, and may remain after a refused push.
4. Record and positively acknowledge the host's dispatch intent, bound to the
   post-mapping checkpoint, selected ref, old/new OIDs and prepared pack. Check
   current host authority before the external effect.
5. Call `send-http!` once. The original expected old OID is also sent in Git's
   receive-pack compare-and-swap. A concurrent remote update is refused by the
   receiver; there is no force-push fallback.
6. Only a positive selected-ref acknowledgement permits recording success and
   advancing the host's remote cursor. A thrown or malformed response is an
   unknown outcome, even if the remote might have applied the update. Preserve
   the intent for explicit reconciliation instead of automatically resending it.

## Incremental identity and limits

Only ancestry of the verified expected remote commit is receiver-known. Such
commits are boundaries and need not be regenerated. Other commits require full
snapshots and canonical reconstruction; any stored Git OID must exactly match
the reconstruction. Imported frontier stubs outside receiver-known ancestry
refuse. This deliberately does not promise arbitrary imported-history export
to a different empty remote.

Native parent relations are unordered, so projected parent OIDs are sorted.
Limits cover graph visits and edges, stage entries, paths, depth, content reads,
object sizes, accumulated output, compressed pack bytes and HTTP response bytes.
The pack writer charges before growing its output buffer and uses bounded
compression chunks. All work shares one operation and deadline. Choose smaller
host limits when appropriate; finite defaults are not a service quota.

Remote publication currently requires SHA-1 and `report-status`, updates exactly
one branch, and accepts only an unambiguous status for that branch. Branch
deletion, force push, automatic retries and credential storage are outside this
API.
