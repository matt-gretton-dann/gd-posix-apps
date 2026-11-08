# Generic VM result-slot initialization

This is an unqualified experiment for #207 and #197. It changes generic result
construction; it does not introduce a new execution backend or remove checks.

Each generic result slot owns a variant whose first alternative is `monostate`.
An empty slot must construct that alternative before any checked access. Clearing
an activation destroys its owning alternatives before the vector is reused.
Those construction, access and destruction rules remain unchanged.

`vector::resize` value-initializes new slots. With an implicit default
constructor, value-initialization first zero-initializes the wrapper, including
the inactive variant payload. A user-provided constructor that explicitly
constructs `monostate` avoids that preliminary zero-initialization. It still
constructs a normal variant with a valid empty discriminant. Defaulting the
constructor would restore the preliminary zero-initialization.

An Apple Clang arm64 Release C++20 inspection of accepted main `64e7ee67`
and a private constructor-only source copy found two `bzero` call sites in the
real generic result-vector resize instantiation, compared with none after the
change. On that ABI a slot occupies 48 bytes; the changed loop initializes its
4-byte discriminant. LTO was disabled for both assembly inspections to obtain
readable output. These are static observations on one toolchain. They establish
neither an elapsed improvement nor the code generation of another platform or
the normal linked IPO build. More loop instructions or different cache behavior
could offset reduced stores.

Correctness review must retain full and verified execution, mixed owning-result
reuse, allocation-failure recovery, recursive activations, and generic fallbacks
when typed execution is enabled. Existing tests exercise these contracts without
depending on inactive payload bytes or a particular standard-library layout.

Before adoption, a separately declared native cohort must compare the exact
candidate with current main under the
[performance acceptance policy](bc-performance-acceptance.md). Keep arithmetic,
math, startup, controls, resources and code-size evidence, including adverse or
inconclusive outcomes. Static store counts do not substitute for that review.
