# Square-root boundary and large-scale coverage

Refs #212, #349 and #197. These tests extend the correctness foundation for
precision-growth experiments; they do not implement or claim a faster algorithm.

`test-number-oracle.py` checks scales 5000 and 10000 for sqrt(2) and sqrt(4),
values immediately below, at and above a 201-digit perfect square, tiny
fractions (including a 1024-place exact square), input precision greater than
the requested precision, and zero at scale 10000. Its integer Newton oracle
constructs the floor independently and verifies both exact inequalities:
`r*r <= x < (r + 10^-result_scale)^2`. The executable's value and declared
scale must match. Existing smaller-scale and negative-error tests remain.

The decimal fixtures are bounded below 25000 digits. Where Python enforces an
integer/string conversion ceiling, this test temporarily raises that ceiling
to 25000 and restores the previous value in `finally`; it does not disable a
process-wide limit permanently. Python versions without that option also work.
The subprocess deadline is a termination safeguard, not a timing assertion.

`test-vm-lifetime.cc` additionally checks sqrt(4) at the two largest scales of
both native 16- and 32-bit models. Its existing isolated replacement allocator
has a thread-local, scoped 1024-byte allocation ceiling. A request must either
return the requested scale or reach an allocation rejected by that ceiling.
Returning a wrapped small scale without growing the coefficient fails. These
four cases therefore exercise guard arithmetic without materializing enormous
coefficients. They do not prove successful computation at maximum native scale
or establish a production allocation limit. The ceiling is inactive outside
the test scope and production allocators are unchanged.

Previously shared snapshots must remain intact when allocation is rejected.
The test deliberately makes no strong-exception-guarantee assertion about the
mutable result. The allocation-balance check includes this path.

The new native-width test fails against the pre-fix source
`9c65f912ac9ff5c32503f8d25d404e439164297f` with “Native square-root scale
wrapped or lost snapshot isolation”, while the #349 implementation passes.
For that check the new helper was compiled with the old source headers and
linked with the old libraries; neither immutable source recipe was modified.
The bounded 8-bit exact digit tests in #349 complement these allocation-boundary
checks. Iteration counts, division dimensions, separate RSS and competitive
precision-growth measurements remain #212 work.
