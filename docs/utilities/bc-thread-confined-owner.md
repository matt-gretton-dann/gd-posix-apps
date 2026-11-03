# Thread-confined coefficient comparison (#446, #197)

This experimental topic branch compares three ownership choices without
removing sharing or changing limb allocation/growth algorithms:

| Compact owner | Thread-confined owner | Count policy |
|---|---|---|
| OFF | OFF | Existing std::shared_ptr fallback |
| ON | OFF | Compact owner with atomic counts |
| ON | ON | Compact owner with ordinary size_t counts |

The options are ENABLE_BC_COMPACT_DIGIT_OWNER and
ENABLE_BC_THREAD_CONFINED_DIGIT_OWNER. The latter defaults ON in this
comparison branch only. It is an explicit experimental build contract, not
an adopted distribution default or a blanket guarantee about C++ clients.
CMake and the headers reject thread-confined ON with compact OFF. Header-only
consumers without CMake definitions retain the shared_ptr fallback. Linked
clients must use the target's propagated layout/count definitions.

With thread-confined counts, every operation on all strong and weak handles
for a coefficient, including copying, observation, reset and destruction, must
remain on one thread. Reading payloads through otherwise independent handles
does not make concurrent ownership operations safe. Choose either atomic
implementation for clients that cannot establish this confinement. No
thread-confined ownership is proposed for the general public API by default.

The inspected CLI boundary is narrower: bc.cc constructs one VM and invokes
its readers/execution on the calling main thread. Production bc numeric/VM
sources do not dispatch numeric owners to worker threads. The POSIX signal or
Windows console handler in vm.cc only sets have_been_interrupted, which remains
an atomic_flag; it does not access coefficient owners. This establishes the
standalone CLI experiment's boundary, not arbitrary embedders or custom stream
callbacks. Future asynchronous VM work would need to revisit this contract.

The atomic and local compact models use the same pointer handle, counter widths,
control-block/vector layout, allocation categories, weak lifetime identity and
last-strong/last-weak cleanup. A compile-time count policy changes increment,
decrement and load operations. Local counts are neither volatile nor secretly
synchronized. No signal/global atomic, standard-library setting, COW boundary,
limb algorithm or math-function recognition is changed.

Both compact policies have direct lifetime, constructor-failure and
self-assignment tests. Concurrent copy/release tests explicitly instantiate
the atomic policy; they are not evidence that local handles are safe across
threads. Public numeric tests for the local build run within the stated
single-thread boundary. Profiling counters retain their existing single-thread
scope. Coverage records the count mode separately; missing options in older
sources remain unknown rather than being relabelled OFF.

The earlier atomic prototype's two local 22-case matrices repeat macOS
slowdowns despite a roughly 15% lower dense-array RSS. Its data and still-running
native collection remain bound to 80712b82 / PR #447. This comparison needs its
own correctness, native full/held-out, elapsed, memory and code-size evidence.
An improvement over the atomic prototype alone is not acceptance relative to
accepted main. The Linux/Windows allowance remains 5% per change; macOS is
unchanged. No performance improvement or adoption is yet claimed.

## Initial correctness validation

All nine separately configured owner/language/profiling configurations pass.
Six ASan/UBSan configurations cover each of the three owners with extensions
ON / prepared reuse OFF and extensions OFF / reuse ON: 329 cases each, with
1,669,327 ON assertions and 1,669,312 OFF assertions. Seven focused CTests pass
in each. The default local-count configuration passes all 93 bc CTest entries
(48 pass, 45 declared skips), including rejection of inconsistent ownership
options. Three profiling configurations, one per owner with extensions/reuse
ON, pass 340 cases / 1,669,392 assertions and profiling CLI checks.

The independent tuner builds in every configuration. Audits preserve actual
layout, count-policy and language definitions for 43 normal / 44 profiling
header consumers, including the tuner, with cache and compile-command hashes.
A separate LLVM 23 C++17 local-owner/vector smoke check passes 100,000
single-thread copy/move/weak-assignment iterations under ASan/UBSan. It is not
whole-library C++17 or concurrent-local-owner coverage. LeakSanitizer is
unavailable on this host. Timing, RSS and fresh native evidence remain pending.
