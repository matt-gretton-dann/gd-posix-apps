# Typed result reconstruction experiment

Refs #700, #694, #696, #207, #200 and #197. This separate topic starts at
accepted main `b0247467ebf007f5d316a1e1ff6eb8ac03e363ef`. It reuses the corrected
prefix-buffer and portable Bessel source foundation, without transferring any
qualification or importing the Word runtime. Prefix tracking remains default
OFF during correctness preflight; explicit OFF keeps ordinary vector cleanup.

## Evidence and change

The frozen `908910c7` source is rejected for adoption. Its first Linux arithmetic
archive confirms about 9.2% full/held recursion losses above the approved cap;
the original jobs and every outcome remain separately retained. Its selected
full-recursion diagnostic executes the same 1,010,013 instructions and 50,000
calls as baseline, but reports 1,444,086 digit moves versus 900,077. These
instrumented observations do not isolate the elapsed cause or establish dynamic
ordinary/profile path identity.

Typed cleanup previously used `number = Number()`, adding a move assignment
from an empty temporary. The new reset destroys the old Number and constructs
the allocation-free default zero directly at the same address. Activation
cleanup ends all slot borrows; escaped values retain their own coefficients.
The complete old object is replaced by the same non-const type using standard
C++20 lifetime operations. No raw mutable storage escapes the owning buffers.
Generic monostate reset and explicit OFF behavior are unchanged. Number
representation/API, arithmetic, scale/base state and numerical outputs are not
special-cased for this change.

The regression was committed before the fix: a warmed 512-producer early-path
fixture executes 30 times and permits at most two ordinary digit moves per
execution. Full checks record 60; the old typed verified path records 90 and
fails the bound. Direct reconstruction passes the same bound. This finite upper
bound permits fewer transfers and contains no timing assertion.

An independent untimed ASan/UBSan/profile probe checks inline and heap-backed
negative fractional values, unique/shared ownership, escaped values and restored
zero/scale. Assignment records 100 moves per 100 resets; reconstruction records
zero. A separate ordinary Apple Clang 21 ARM64 Release wrapper probe, with IPO
disabled and wrappers forced out of line, lowers assignment to 83 instructions,
10 conditional branches and an 80-byte stack frame, versus 26 instructions,
two conditional branches and 32 bytes for reconstruction. This is selected
wrapper code generation, not complete VM code generation, other platforms or
elapsed performance. It does not prove the original recursion loss is fixed.

## Validation and qualification

Keep logical backing-slot initialization separate from reset construction,
relocation, allocator traffic and elapsed cost. The existing selected counters
retain their meanings; every reset visit remains counted. See the
[buffer ownership and counter contracts](bc-prefix-clear-results.md).

Exercise generic Release, typed and explicit OFF sanitizers, checked/verified
execution, escaping owners, mixed result kinds, recursion, allocation failures,
signals and CLI/numerical oracles. A new native preflight must retain actual
compiler policies, VM objects, images, source inputs and logs for this source.
No local or historical result substitutes for final-head native checks.

The immutable `e563159a` preflight completed all 49 hosted checks and all 38
original native archive reviews. Its prefix modes retain actual VM objects,
images, compiler definitions and eleven source inputs; profile fixtures pass
the same move bound. This establishes correctness evidence for that revision,
not elapsed performance or qualification of a later revision.

Qualification collectors retain each ordinary and diagnostic image and its
source/build policy. Before sampling, require actual prefix ON for candidate
roles and implementation absence for the accepted baseline. Check the VM
compiler command or Visual Studio Release definitions against the CMake option,
retaining the VM, CMake and header inputs. Explicit OFF is distinct from an old
source without the implementation. Matching policies establish compiled
selection; they do not prove dynamic ordinary/profile path identity. The
collector fixtures run through CTest as well as directly.

Before adoption, declare and freeze this candidate's own complete three-platform
full/held arithmetic, math, startup and resource study against accepted main.
Keep the approved policy, caps, Howard-margin exception, competitive targets,
controls, precision, duration, one-shot, memory and code-size requirements. Do
not retry unfavorable cohorts, omit classes, pool prior observations or multiply
isolated ratios. No local production elapsed benchmarks are run.

The ten-day TOTAL goal clock includes implementation, queues, collection and
review. Stop qualification at 864,000 seconds, preserve results and leave unmet
issues open. Keep at least 20 GiB free. Standard C++20, CMake 3.15 and the generic
fallback remain required.
