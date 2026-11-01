# Accepted reusable activation storage

PR #326 merges source `569b1cdcc36c6c5493038cf22bc6137794640595`.
Native run 41282012761 compares exact decimal-conversion predecessor
`00365f2f75324278024e665ba025aa47abbf6d70` with that source. All 1450
invocations per OS pass exact output, status and deadline checks. Artifact
SHA-256 values and source identities are recorded in the adjacent audit JSON;
complete raw samples, recipes, programs and decisions remain inside the ZIPs.
No OS has a confirmed regression under the provisional native policy.
Startup and noisy intervals remain inconclusive, rather than passes.

The original Windows low-comparison loss remains in
[initial-integrated-native](../initial-integrated-native/README.md). The changed
source adds an MSVC-only noinline policy to the limb comparator, retaining the
generic body and compiler policy elsewhere. In this new run, Windows high/low/
equal comparison ratios are approximately 0.997/0.996/0.991. The low comparison
interval [0.970,1.085] is inconclusive. This is a new-source observation, not a
claim that the previous regression was measurement error or a proven inlining
cause. The separate non-MSVC executable identity check is already archived in
this issue's evidence directory.

The [matched local matrix](../../bc-197-integrated-matrix/README.md) preserves
all original sources and separate counter runs. Its adjacent frame stage has
call ratios 0.801 full / 0.797 held-out, together with roughly 3–4% loop and
Fibonacci costs. The earlier standalone held-out division loss and integrated
Windows loss remain visible. Counter tests establish that repeated calls of
the same shape stop allocating frame/result storage after warm-up. They do not
measure process RSS, host stack consumption or a guaranteed recursion depth.

## Explicit call-stack decision

Retain host-recursive control flow for this change. Contiguous argument/local
storage and reusable per-depth result buffers address the observed allocation
cost with measured call improvements. An explicit dispatcher stack would also
need to encode pending instruction positions, return destinations, saved local
ranges and exception/interrupt unwinding. It changes control-flow and resource
failure behavior without an established additional performance benefit in these
measurements. This is a design evaluation, not a benchmark of an unimplemented
alternative, and does not claim that host recursion is unbounded or optimal.

Deep recursion/resource limits remain the separate #184 contract work. The
current supported recursion, mutual recursion, scalar/array aliasing, argument
order, redefinition and saved-special-variable behavior is preserved by the VM
and CLI tests. Function bodies remain persistent; redefining a function replaces
its verified code owner and its base-dependent literal cache. No recursion-depth
promise is added by closing #208.
