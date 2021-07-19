# Implementation of bc utility

This document gives an overview of the implementation of the `bc` utility.

The code is a fairly standard (hand-written) lexer and parser that generates an instruction stream
which is interpreted by a Virtual Machine.

There is also the implementation of arbitrary precision arithmetic to consider.

The majority of the code is implemented as a stand-alone library (`libbc`) to ease unit testing of
the various components.

## Non-POSIX Extensions

To enable use of the gavinhoward-bc testsuite a small number of extensions have been added as
follows:

If built with non-POSIX extensions enabled the following functionality is added:

 * `-q` command line option.  This does nothing in `bc` but is a GNU bc option to stop a copyright
   header being printed.

 * `-g` command line option.  This causes function invocations to push `ibase`, `obase`, and
   `scale` onto a stack (i.e. treat them as locals to functions).

 * `halt` statement.  This does a `quit` when executed.  Unlike `quit` which quits the code when
   parsed (which is virtually never what you want).

 * Numbers which are written over two-or more lines may have white-space at the start of the
   continuation lines.

 * If POSIX extensions are enabled numbers with a magnitude of less than 1 will not have a 0 printed
   before the radix pointed.  When disabled a character will always be printed before the radix
   point.

 * Length can be called on an array slice (`length(a[])`).  This returns the number of elements in
   the array.

 * The absolute form of a number may be calculated using the `abs()` expression.

 * `return foo;` is valid.

 * No newline needed after opening brace of a function definition.

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

The Virtual Machine executes a 'register'-ish based instruction set.  Each instruction has two
input registers and an output register.  When an instruction wants to use the result of a previous
instruction it references that instruction.

The data types for input registers are:

| Type | Description |
| :--- | :---------- |
| `std::string` | Either a string literal or the textual representation of an arbitrary precision number. |
| `Instruction::Stream` | Identify standard output versus standard error. |
| `Instruction::Offset` | Signed offset to the result of another instruction. |
| `Location` | Source location. |
| `VariableMask` | Mask of variables and arrays, used for identifying locals in a function definition. |
| `unsigned` | Used for an exit code. |
| `Letter` | Function name. |
| `Variable` | Reference to a variable. |
| `Array` | Reference to an array. |

Numbers are not converted from strings until instructions are executed as the the interpretation of
a number is determined by the `ibase` at the time of execution.

All offsets are relative to the current instruction.  (This was a design decision as it means saving
function definitions is simply a matter of copying the instructions and no fix up is needed).

Data types for results are:

| Type | Description |
| :--- | :---------- |
| `Number` | An arbitrary-precision number. |
| `std::string_view` | A string. |
| `ArrayValues` | All the values in an array - only used in parameter passing. |
| `Variable` | Reference to a variable. |
| `Array` | Reference to an array. |
| `ArrayElement` | Reference to an element of an array. |
| `Ibase` | Reference to ibase. |
| `Obase` | Reference to obase. |
| `Scale` | Reference to the scale global. |

Instruction summary:

| Opcode              | Result type  |  Operand 1   |  Operand 2 |  Description                               |
| :------------------ | :----------  | :----------  | :--------- | :----------------------------------------- |
| eof                 |              |              |            | end of file, execution stops.              |
| print               |              | Offset       | Stream     | Print the value at op1 to stream op2.      |
| quit                |              | unsigned     |            | Quit - using exit code Op1.                |
| string              | String       | String       |            | A string value                             |
| number              | Number       | String       |            | A number value                             |
| variable            | Variable     | Variable     |            | A variable                                 |
| array               | Array        | Array        |            | Array op1                                  |
| array_element       | ArrayElement | Array        | Offset     | Element op2 of Array op1                   |
| scale               | Scale        |              |            | Scale variable                             |
| ibase               | Ibase        |              |            | ibase variable                             |
| obase               | Obase        |              |            | obase variable                             |
| add                 | Number       | Offset       | Offset     | Op1 + Op2                                  |
| subtact             | Number       | Offset       | Offset     | Op1 - Op2                                  |
| negate              | Number       | Offset       |            | -Op1                                       |
| multiply            | Number       | Offset       | Offset     | Op1 * Op2                                  |
| divide              | Number       | Offset       | Offset     | Op1 / Op2                                  |
| modulo              | Number       | Offset       | Offset     | Op1 % Op2                                  |
| power               | Number       | Offset       | Offset     | Op1 ^ Op2                                  |
| load                | Number       | Offset       |            | Load value stored in named-expr op1.       |
| store               | Number       | Offset       | Offset     | Store value Op2 into named expression Op1. |
| scale_expr          | Number       | Offset       |            | Calculate scale(op1)                       |
| sqrt                | Number       | Offset       |            | Calculate sqrt(op1)                        |
| length              | Number       | Offset       |            | Calculate length(op1)                      |
| equals              | Number       | Offset       | Offset     | 1 if op1 == op2, 0 otherwise               |
| less_than_equals    | Number       | Offset       | Offset     | 1 if op1 <= op2, 0 otherwise               |
| not_equals          | Number       | Offset       | Offset     | 1 if op1 != op2, 0 otherwise               |
| less_than           | Number       | Offset       | Offset     | 1 if op1 < op2, 0 otherwise                |
| branch              |              | Offset       |            | Unconditional branch to op1                |
| branch_zero         |              | Offset       | Offset     | Branch to op2 if op1 is 0.                 |
| return_             |              | Offset       |            | return from function with value Op1        |
| call                | Number       | Letter       | Location   | Call fn op1, op2 is source loc of call     |
| push_param_mark     |              |              |            | Push fn separator marker onto param stack  |
| pop_param_mark      |              |              |            | Pop fn separator marker from param stack   |
| push_param          |              | Offset       |            | Push parameter onto param stack            |
| pop_param           | Number       |              |            | Pop scalar parameter from param stack      |
| pop_param_array     | ArrayValues  |              |            | Pop array parameter from param stack       |
| function_begin      |              | VariableMask | Location   | Start of function definition               |
| function_end        |              | Letter       | Offset     | End of function definition                 |


#### Loading and Storing Values

Loading values requires the instruction stream to contain instructions to reference
the object to load, and then to load the value from it.  For example:

```
    variable    v
    load        -1
```

After executing the result of the load will conatin the value stored in the variable `v`.

Storing values is similar:

```
    number        "1"
    array_element a, -1
    store         -1, -2
```

After execution of the above `a[1]` will contain the value 1.

#### Function Definitions and calls

Parameters are passed on the *parameter stack*.

Function calls should begin by executing a `push_param_mark` instruction - which indicates the start
of a new set of function parameters.  It should then call `push_param` on each parameter in turn
(whether a `Number`, or an `ArrayValue`), in order from left-to-right as specified in the function
call itself.  Then to call the function it should execute the `call` instruction - the result of
which is the value of the executed `return_` instruction - or zero if no `return_` was executed.

**Note:** There must be exactly one `push_param_mark` for every `call` instruction - even if the
function being called takes no parameters.

Function definitions are demarkated by `function_begin` and `function_end` instructions.  Within a
function call parameters should be retrieved from the parameter stack by executing `pop_param` and
`pop_param_array` instructions.  Again this should be in order from left-to-right as in the function
definition.

**Note:** that yes this is not a stack but a list of the parameters for a particulart function - but it
is a stack of parameters...

Once all parameters have been read a function must execute a `pop_param_mark` instruction.

**Note:** Every function must execute a `pop_param_mark` instruction exactly once - even if there
are no parameters passed in.

#### Static Checking

Before executing a set of instructions the virtual machine does some basic static checks to try to
catch obvious issues with invalid streams.

The checks include:

 * Correct data types are stored in instruction inputs.
 * Offsets point within the instruction stream.

Future improvements to checks are needed and include:

 * Check that offsets reference instructions that produce the correct type of result.
 * Improve checks around ensuring `push_param_mark` and `pop_param_mark` are present where they
   should be.

See [#187](https://github.com/matt-gretton-dann/gd-posix-apps/issues/187)

#### Future changes/improvements

 * Encode sign in instruction name of things with offsets
   - load/stores only refer to previous locations
   - Branch needs to have branch forward, branch backwards
   - Branch_zero should only branch forward
 * Parameter passing needs to be made more robust, and error proof (see
   [#187](https://github.com/matt-gretton-dann/gd-posix-apps/issues/187)).
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
