# bc component measurements

`bc-components` separates selected frontend, conversion and arithmetic API costs
from the full process corpus (#200). It is a developer measurement tool, built
with unit tests or explicitly with `ENABLE_BC_COMPONENT_BENCHMARKS=ON`.

```sh
cmake -S . -B build-components -DCMAKE_BUILD_TYPE=Release \
  -DENABLE_BC_COMPONENT_BENCHMARKS=ON
cmake --build build-components --target bc-components
printf '12345678901234567890\n9988776655\n' | \
  build-components/src/bc/bc-components multiply 10000 126
```

Arguments are component, iteration count (1 through 10,000,000), and an explicit
multiplication split point in limbs (1 through the native limb radix). Stdin
supplies a fixture followed by EOF, with a 1 MiB limit. Numeric components use a
signed decimal on each of the first two lines. Control and conversion components
need only the first number. Parse takes an ordinary bc program instead. A parent
runner must set a finite process deadline and reject nonzero exit or stderr.

The one-line JSON reports component, iterations, split point, limb width,
unsigned checksum, final exact numeric result (joined continuation lines), and
steady-clock elapsed seconds. Parse reports an instruction count instead of a
numeric result. Instrumented builds identify themselves and emit `seconds:null`;
those runs cannot establish a timing improvement. Record source, compiler/library,
flags and executable hash separately, as for the process corpus.

## What each interval includes

Fixture loading, initial numeric operand construction and final arithmetic-result
formatting are outside the interval. Operation selection occurs once outside the
iteration loop. Each arithmetic iteration starts with the same immutable lhs
and consumes the result length, retaining realistic copy-on-write/reset costs.

| Component | Work inside the interval |
| :--- | :--- |
| `control` | Copy/reset lhs and consume its numeric length. |
| `add`, `subtract`, `multiply`, `divide` | The control work plus one Number API operation. Multiply/divide use target scale zero, preserving the API's normal operand-scale rules. |
| `conversion-in` | Validate/construct the signed decimal operand, reset the result, and consume its numeric length. |
| `conversion-out` | Construct an output string stream, format the prepared Number in decimal, and capture/count the raw string including continuation lines. |
| `parse` | Construct reader, lexer and parser; parse all input items through EOF; count instructions and check for fatal parser output. No VM is constructed or executed. |

Length consumption itself can scan digits. Stream construction/capture and signed
input validation are included in the conversion components. These are explicit
API/component intervals, not isolated assembly kernels. Keep the control results
visible; subtracting two noisy medians does not recover a pure instruction cost.
Invalid numeric inputs, zero divisors, read-time quit and fatal parser streams
are rejected. Parse accepts `1/0` because it performs no calculation.

## Correctness and interpretation

`bc/components` checks independent Python integer results across basic/mixed
multiplication split points, signed fractional arithmetic, input/output
conversion with long continuation lines, and argument/resource boundaries.
The parse check proves that division by zero is parsed without execution and
that a second statement increases the complete instruction count. Timings are
never asserted by these tests. Full VM correctness remains gated by the broader
arithmetic, parser and CLI oracle suites.

Use warm-up, fixed operand seeds, repeated interleaved base/candidate runs,
paired uncertainty and held-out inputs before claiming an improvement. Preserve
raw component JSON alongside the independent oracle and the end-to-end result.
Component evidence can identify where work occurs; it cannot substitute for a
whole-interpreter win against Howard or for cross-platform measurements.
