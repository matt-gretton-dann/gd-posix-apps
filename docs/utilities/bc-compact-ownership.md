# Compact coefficient ownership (#446, #197)

The prototype replaces the two-pointer shared heap handle in DigitStorage
with one pointer to a coefficient-specific control block. Inline limbs,
coefficient algorithms, explicit COW detach and retained vector capacity are
unchanged. Smaller handles may reduce numeric value and VM-frame footprint;
neither layout alone nor an allocation counter establishes a speedup.

ENABLE_BC_COMPACT_DIGIT_OWNER selects the compact atomic owner; OFF retains
the existing std::shared_ptr implementation as a generic control and fallback.
CMake propagates the selected layout through the shared header-mode interface
to libbc clients and the independent multiplication tuner. Header-only consumers
without that definition retain shared_ptr. Linked clients must use libbc's
CMake usage requirements so their numeric layout agrees with the library.

Strong and weak counts are atomic. Independent copies retain the existing
ownership safety; neither implementation makes concurrent mutation of one
coefficient or handle safe. A non-atomic comparison is a separate experiment
requiring an established thread-confinement contract. Signal/global atomics
and standard-library thread configuration are not changed.

The block co-locates counters and vector metadata. Its vector is constructed
in a union member and explicitly destroyed on last-strong release. One implicit
weak reference protects that transition. Remaining weak observations keep only
the block alive, never the vector or limb allocation; last-weak release frees
the block. Identity compares live control-block addresses. An expired observer
still owns its block, preventing address reuse from matching a later lifetime.
There is no weak-to-strong promotion, aliasing-owner or custom-deleter API.

This changes metadata ownership, not co-allocation of a variable-capacity limb
buffer. Allocation profiling tracks both the selected control-block allocator
and limb allocator, including failed construction. Its counters remain thread
confined, so concurrent ownership tests run with profiling disabled. These
are selected storage counters, not process-wide malloc measurements.

Before integration, the original shared_ptr runtime passes the new public
weak-assignment and retained-limb-lifetime checks: 60 assertions in six trait
cases, and the full storage selection's 678 assertions in 27 cases, under
ASan/UBSan with profiling enabled. The test commit is 71c81f59. LeakSanitizer
is unavailable on the local host.

Four ASan/UBSan configurations pass 327 unit cases and six focused CTests each:
both ownership choices with extensions ON / prepared reuse OFF and extensions
OFF / reuse ON. Unit assertions are 1,669,208 and 1,669,193 respectively.
Two separate profiling configurations, one per owner with extensions/reuse ON,
pass 338 cases / 1,669,272 assertions and the profiling CLI checks. The default
compact-owner configuration passes all 92 bc CTest entries (47 pass, 45 declared
skips). The independent tuner builds in every configuration; audits verify 43
normal / 44 profiling header consumers and their actual layout/language flags.

Dedicated control-block tests cover failed construction, last-strong destruction,
expired identity, self-assignment and independent concurrent copy/release.
The first uncommitted prototype build found a missing weak reset operation used
by cache cleanup; its failed patch and diagnostics remain retained separately.
The corrected owner has that operation and a dedicated check. Sanitizer success
is not a claim of race-detector coverage for the entire numeric library.
A separate LLVM 23 ThreadSanitizer stress executable, compiled as C++17, passes
400,000 copy/move/weak-assignment iterations across four threads using the actual
owner header and a vector coefficient. This checks that narrow ownership path,
not the complete numeric library or generated native code.

Local AppleClang/libc++ layout probes measure native Number at 40 bytes with
shared_ptr and 32 with the compact owner. The execution-result variant shrinks
from 48 to 40 bytes; its type declaration is copied exactly from vm.cc for this
probe. These are local type sizes, not process RSS or other-platform results.

Adoption requires fresh source-bound full/held-out native elapsed-time evidence,
startup, memory and code-size observations, with slower and inconclusive cases
retained. The authorized per-change Linux/Windows allowance is 5%; the macOS
requirement is unchanged. No performance improvement or adoption is claimed
by this prototype. The rejected #439 entry variants retain their own source
identities and are not part of this experiment's baseline.
