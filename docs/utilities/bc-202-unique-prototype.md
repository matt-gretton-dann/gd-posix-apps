# Unique heap ownership experiment

Refs #202 and #197. Experimental branch only; this is not an adoption decision.
Keep the existing three inline limbs and vector arithmetic, but eagerly copy
heap vectors through `unique_ptr`. Copy operations may allocate and throw, so
they no longer promise `noexcept`. Moves still transfer ownership. Snapshot
values remain independent without detaching on mutation.

Run independent arithmetic/VM/output oracles before timing. Shared-owner count
and COW-clone profiling assertions describe the production COW representation
and are not acceptance oracles for this prototype. Heap owner allocation is
not tracked by the existing shared-factory counters; report actual RSS and
release timings, not incomparable selected byte totals. Retain a production COW
baseline and do not adopt this branch without matched all-class evidence.
