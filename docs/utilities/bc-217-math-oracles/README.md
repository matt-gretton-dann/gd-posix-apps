# Certified reference groundwork for the portable math library

Refs #217 and #197. This delivers independent accuracy-reference machinery,
not runtime math functions or a speed/accuracy claim for GD bc. The existing
-l stub still only initializes scale. Implementations, loading/state tests,
portable-source runs, algorithm error proofs and matched benchmarks remain.

## Chosen normative baseline

Use POSIX.1-2017, The Open Group Base Specifications Issue 7, 2018 edition.
The supplied `susv4-2018` standards archive contains `utilities/bc.html`;
the consulted HTML has SHA-256
`3f76dbebd1f6b1e337d60dc63fdb199e7f1aace9b5dd47692e3fb94589def362`.
The copyrighted standards text is not copied into this repository. The public
[bc specification](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/bc.html)
identifies the normative contract; later editions were not compared here.

The six library names are s, c, a, l, e and j. The first two use radians;
j takes an integer order and a real argument. With -l, functions must be
available before application input and scale starts at 20. Each function's
result scale is the entry scale, and that register is restored on return.
Out-of-domain arguments have undefined POSIX behavior: an implementation's
chosen diagnostics must be distinguished from mandated behavior. The integer
order interface does not establish a required truncation rule for nonintegers.

Function constants follow invocation-time ibase. Automatic scalars/arrays are
pushed on entry and restored on return; called functions see the current local
binding until they shadow it. Ordinary library source must account for these
rules, preserve caller state and document any helper namespace. These verified
language rules permit portable workspaces; they do not authorize hidden VM state.

The <=10^-entry_scale absolute-error contract is the project's stronger
requirement, not an accuracy guarantee specified by this POSIX edition.

## Exact decision procedure

The standard-library-only `scripts/math_oracle.py` parses plain decimal output
as exact rational numbers. No binary floating-point conversion or decimal
context rounding enters the decision. For a certified reference interval [L,U],
candidate y and delta = 10^-S:

- Pass only when max(abs(y-L), abs(y-U)) <= delta.
- Fail only when the distance of y from the entire interval exceeds delta.
- Otherwise refine the reference. An ambiguous interval is not a failure.

One-ULP equality is inclusive. Reversed bounds, inexact operand types and
unsupported resource limits are errors. The test-only nearest-decimal helper
constructs checker fixtures; it implements no mathematical runtime function.

## Independent reference generation

[python-flint's Arb interface](https://python-flint.readthedocs.io/en/latest/arb.html)
provides rigorous ball arithmetic. The generator constructs each exact input
rational with fmpq, converts it to an enclosing arb, evaluates the requested
function and exports outward endpoints as exact dyadic rationals. Input
conversion uncertainty is propagated, including large trigonometric arguments.
No printed decimal approximation, host libm value, Howard answer or agreement
between precisions is used as a reference certificate.

The initial precision estimate is only an estimate. Actual finite endpoint
width must be at most 10^-(S+16); otherwise precision doubles until that bound
holds or the configured resource budget is exhausted with an error. This
budget controls reference tightness, not the runtime library's guard precision.
The generator restores the previous FLINT precision even on failure.

Pinned test-generation dependencies: python-flint 0.9.0, bundled FLINT 3.6.0.
The committed report records these versions and hashes of the generator and
input corpus. Scoped checkout attributes and explicit LF byte writes preserve
these identities across Windows regeneration. A standard-library-only CI check consumes the report, so FLINT
is neither a runtime dependency nor required to run the committed checker.

```sh
python3 -m venv /tmp/bc-math-reference
/tmp/bc-math-reference/bin/python -m pip install -r src/bc/scripts/math-oracle-requirements.txt
/tmp/bc-math-reference/bin/python src/bc/scripts/generate-math-oracles.py \
  src/bc/fixtures/math-oracle-cases.json /tmp/math-oracles.json
python3 src/bc/test-math-oracle.py
```

The fixture generator supports explicit precision/guard resource limits. Python
integer/string conversion uses a finite one-million-digit limit where available;
the checker bounds requested scale separately. Broader input/scale regimes can
require a larger reference budget and additional fixtures. No failed reference
calculation may be silently accepted or compared through a float fallback.

## Checked scope and limitations

The corpus has 48 exact input regimes and 336 function/scale enclosures at
scales 0,1,2,20,100,1000,2000. It includes exact zeros/ones, signs, tiny inputs,
log near one, large exponentials, huge and near-quadrant trigonometric arguments,
negative Bessel orders/arguments, turning cases, order/argument-dominated cases
and a near-root value. Every endpoint is an exact rational outward bound.

The checker validates corpus identity/coverage and width, accepts nearby
certified values and rejects deliberate +/-3-ULP faults for every enclosure.
Separate unit cases exercise ambiguous intervals, inclusive boundaries, invalid
numeric text/types, signs and resource errors. These check the oracle and its
fault sensitivity; they do not exercise missing GD math functions. A runtime
library still needs its own all-regime error/termination argument and observable
scale/state, loading order, user-redefinition and portability tests. Finite
fixtures alone cannot establish the required universal accuracy contract.

## Validation of this groundwork

Twelve stdlib-only unit groups pass on Python 3.9.25 and 3.15.2. The checker
is registered and passes with extensions ON and OFF. The Release bc target
builds; its runtime source is unchanged by this groundwork. Regeneration with
the pinned Python 3.13 tool environment reproduces the committed report bytes.
Precision-budget exhaustion and an injected reference exception both preserve
the prior precision and fail explicitly. Native cross-platform CI remains
required for the PR; no runtime math-function test is claimed by these checks.
