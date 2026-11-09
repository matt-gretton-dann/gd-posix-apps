# Live-prefix variant coefficient experiment (#705)

This is an unqualified proposal to enable compact coefficients for #197/#207.
Correctness preflight used default OFF; the frozen qualification tests the
proposed default ON before adoption.
It changes no arithmetic algorithms, language semantics or ownership contract.
`ENABLE_BC_COMPACT_DIGITS=ON` selects `CompactDigitStorage` only when its actual
object size is smaller than the accepted `DigitStorage`. Other limb widths or
architectures retain the accepted representation. Explicit OFF is always the
portable control. CMake publishes the selection through `bc-language-mode`,
including the independent header-only multiplication tuner. Every consumer of
Number must use the same option; it is a public layout choice.

The accepted implementation remains unchanged. The experiment overlays inline
limbs and a standard shared_ptr using std::variant-managed lifetimes. Heap
coefficients keep the same allocator-aware vector and shared ownership. There
are no pointer tags, shared_ptr layout assumptions, custom reference counts,
thread-confinement assumptions or compiler-specific lifetime workarounds.

Only the live inline prefix is initialized and copied. Inline's copy operations
are deleted: variant copies of that alternative cannot accidentally inspect an
uninitialized suffix. The outer storage explicitly copies its live prefix or
constructs a shared owner. Promotion allocates and fills the replacement before
changing the active alternative; copy/move/reset alternative constructors do
not throw. A move leaves the source absent. Nullable and owned-empty states,
shrink-without-relocation, shared-small clone demotion, weak lifetime identity
and explicit caller-side COW remain the accepted API contract. A weak observer
keeps the control block, never a live coefficient vector, after last-strong
release. No coefficient storage can escape into an inactive inline alternative.

Corrected native preflight at `b8ff3cef` records storage 32 -> 24 bytes and
Number 40 -> 32 bytes on Linux, macOS and Windows. uint8/uint16 storage stays
24 bytes and selects the accepted backend on these hosts. These are object
layouts, not measured throughput, RSS or a universal ABI guarantee. All ten
native configurations passed; all 46 checks and 36 original archives are
preserved and reviewed separately from enabled qualification.

Earlier variants in [the #202 report](../bc-202-inline-storage/README.md) lost
on heap-heavy native square-root and sparse-product cases. Its explicit union
also failed optimized correctness. Those outcomes remain adverse evidence.
This experiment combines variant-managed lifetimes with live-prefix-only
transfers and current arithmetic/workspace code; that distinction establishes
neither a causal explanation of past losses nor a future speedup.

Focused tests cover all limb widths, absent/inline/heap copy and move transitions,
self/aliased assignment, newly exposed limbs after copy/shrink/reset, retained
heap ranges and capacity, weak identity and expiry, shared-small demotion,
failed oversized promotion and noexcept contracts. A separate profile test
checks allocation-free inline copies, allocation-free shared copies, explicit
clone separation and final strong/weak allocation balance. Native correctness
exercises Number, VM, divisor-cache, CLI, signals and lifetime fault handling
in ordinary, diagnostic and fallback configurations. Optimized
uninstrumented correctness is required as well as sanitizers.

Adoption requires a separately declared immutable full/held-out native
Linux/macOS/Windows arithmetic, portable math, startup and resource cohort with
unchanged current acceptance rules. No local production elapsed benchmarks,
native CPU sampling, favorable retries or pooling are authorized. Static size,
compilation, counter and correctness results are distinct from qualification.
The original square PR #704 cohort and source remain untouched. All work,
queues and review must fit the user's 864,000 total recorded #197 seconds and
maintain 20 GiB local free space; unmet goals remain open at the cap.

The first native source `3abc25a2` failed enabled Windows compilation before
runtime tests: MSVC rejected dependent sizeof arguments directly in the
selection alias (C2027). The original source, failed artifact and diagnostics
remain preserved. A selector class defers those layout queries until class
instantiation, retaining the identical selection rule. Native confirmation is
required; local Clang success alone did not detect the original failure.

Original native setup also exposed Bash 3's empty-array/nounset behavior on
macOS and GCC 13 sanitizer warnings inside Catch2's standard-library regex
implementation. Test-first shell fixtures execute the actual workflow argument
block against a mock cmake command. Ordinary builds retain their native
compiler; the additional sanitizer configuration explicitly selects Clang,
records that toolchain, and disables IPO for sanitizer instrumentation. The
ordinary options array retains explicit AUTO IPO so strict Bash 3 sees a
nonempty array. No warning suppression or production compiler change is used.

Qualification tooling carries the ordinary/profile resource collectors and
their fixtures from source `5a805f0a`, without importing its square implementation
or changing this backend's default. The tooling retains actual images, source
and build policies, allocation events and separate cold RSS observations.
The compact policy binds the configured option to the VM, remainder, CLI and
independent tuner's compilation commands, or all three native Visual Studio
projects. It retains the CMake cache and five source inputs, rejects missing,
duplicate or overridden definitions, and distinguishes legacy absence from an
explicit OFF configuration. Arithmetic and math controllers check candidate
and baseline roles before elapsed work, including separate profile builds.
Resource comparisons require matching ordinary/profile selections and source
hashes. The opt-in flag does not itself prove that the size condition selects
the variant, or establish dynamic identity of ordinary and diagnostic paths.
The prospective immutable study protocol remains required before launch.

Resource representatives use the elapsed arithmetic corpus seed: 197200 full,
197201 held-out. A test-first regression caught the prior default seed in
held-out resource generation. Common resource and elapsed cases now retain
identical program and oracle dictionaries; additional lifetime/cache probes
remain explicitly separate diagnostic cases. Frozen #703 data is unchanged
and records the earlier seed mismatch as a resource-correspondence limitation.
