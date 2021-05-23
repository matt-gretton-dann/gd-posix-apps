# Implementation of bc utility

This document gives an overview of the implementation of the `bc` utility.

The code is a fairly standard (hand-written) lexer and parser that generates an instruction stream
which is interpreted by a Virtual Machine.

There is also the implementation of arbitrary precision arithmetic to consider.

The majority of the code is implemented as a stand-alone library (`libbc`) to ease unit testing of
the various components.

## Lexing & Parsing

There is not much to say about the lexer and parser as they are fairly standard.

Numbers are kept stored as strings until executed as the spec says that the ibase at the time of
execution determines the interpretation.

### Error handling

The parser's handling of errors is to stop parsing immediately, and to not to try to do any recovery.

There are some obvious requirements of the language which may be easy to recover from.  For instance:

 * `return (x)` - the brackets are a requirement
 * compulsory newline after opening `{` of a function definition.

See [#185](https://github.com/matt-gretton-dann/gd-posix-apps/issues/185) covering these.

### Code Generation

Code is generated in a straight line which leads to some interesting VM-code generation for `for()`
loops:

```
for (<init>; <rel>; <update>) <body>
```

is generated as:

```
    <init>
begin:
    <rel>
    branch_zero exit
    branch      body
update:
    <update>
    branch      begin
body:
    <body>
    branch      update
```

### Virtual Machine Instructions

#### Future changes

 * Encode sign in instruction name of things with offsets
   - load/stores only refer to previous locations
   - Branch needs to have branch forward, branch backwards
   - Branch_zero should only branch forward
 * Parameter passing needs to be made more robust, and error proof
 * Considering caching the number encoding as we assume `ibase` won't change that often, and well
   written functions will set it to `A` at the start of the function before doing anything odd.
 * Also maybe handle `0` and `1` as special as we know they are those numbers - no matter the base.

## Arbitary Precision Arithmetic

The header file [number.hh](../../src/bc/number.hh) contains the classes which deal with arbitrary
precision arithmetic.

### Number representation

The main class is `GD::Bc::BasicNumber<NumberTraits>`, and this represents an arbitrary precision
number.

Numbers are stored in a manner that is similar to IEEE 754 floating point - except that the storage
space is not fixed.

Given a number there are four components:

 * `BASE` - the base used, this is a multiple of 10.
 * `DIGITS` - the mantissa, stored as a vector of numbers in base `BASE`.
 * `SIGN` - the sign.
 * `SCALE` - the scale - how many decimal digits after the decimal point.

Then any number can be represented as:

```
SIGN * DIGITS * 10 ^ (-SCALE).
```

The `DIGITS` are stored in a vector of numbers in the given base.  The base is chosen to be the
largest possible whilst still allowing widening arithmetic in single operations.

So for example on 64-bit machines the base is `1000000000` which is the largest power of 10 to fit
in a `uint32_t`.  All basic operations on a `uint32_t` have results that fit in a `uint64_t`.

The current implementation limits `SCALE` to also be the same size as `BASE`.
This could be reconsidered and limit the scale to the largest unsigned value possible.  On 64-bit
machines this probably won't make a difference (with the max scale being just under a billion), but
on 32-bit machines it is much lower (9999). See
[#184](https://github.com/matt-gretton-dann/gd-posix-apps/issues/186).

Similarly the total number of digits is limited to `BASE`, but should be at least
`log10(BASE) * (1<<8) <<(sizeof(largest_uint))` - see
[#184](https://github.com/matt-gretton-dann/gd-posix-apps/issues/184).

We store the digits behind a shared_ptr so that we can leave it up to the C++ Standard Library to
manage memory as it sees fit.  We currently do this on the digits only so that `-10` and `10` may
share the same digits (but do they ever really?).

### Algorithms

#### Division

Use Knuth Algorithm D from Section 4.3.1 of TAOCP.  However, we choose the initial scale factor
slightly differently as the one in Knuth doesn't act as expected.

#### Multiplication

We make use of both the classic long-multiplication and Karatsuba algorithms.

TBD: Fill in Karatsuba definition

#### Power

We make use of the fact that x^2n = (x^2)^n to enable us to make the power function run in O(lg n)
multiplications.

For raising to a negative power we calculate P = x ^ |n| first before doing 1/P to the appropriate
scale.  This ensures we get the correct answer.

#### Square Root

We use the basic Newton-Raphson recursion at a larger scale than we want to ensure we get the right
answer and then scale down.

#### Benchmarking

Better at large scales than GNU bc - but not at low scales.  Overheads in shared_ptr and iterators.
I think due to trying to be thread-safe which we don't need.

But our cache locality and branch prediction is much better.

#### Future directions

 * [#184](https://github.com/matt-gretton-dann/gd-posix-apps/issues/184): length() changes
 * [#186](https://github.com/matt-gretton-dann/gd-posix-apps/issues/186): scale() changes
 * Roll our own shared_ptr and vectors which are simpler and have fewer overheads.
 * Benchmark better ways of doing split for multiplication (add lengths?/multiply lengths?)
