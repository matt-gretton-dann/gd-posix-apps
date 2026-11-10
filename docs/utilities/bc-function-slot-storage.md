# Shared storage for owned and deferred function definitions

Refs #217 and #197. This is an unqualified storage experiment based on
accepted main `3beebb14`. It does not change the portable math source,
Number representation, bytecode encoding, Windows loader default or the
frozen Bessel study in PR #712. No elapsed benefit is established.

Image-enabled builds previously reserved an optional owning definition and
a separate deferred descriptor for every function name. A name cannot need
both at once: it is absent, deferred, or decoded. The new internal
`FunctionSlot` stores those alternatives inline in one tagged union.
Source-loader builds retain their existing optional owning definitions;
cross builds continue to use that generic source-loader fallback.

An owning definition is fully constructed before assignment to the slot.
The slot requires nonthrowing moves/destruction, so replacing a pending
descriptor or old owner cannot leave an unowned, partially committed value.
The first-call loader copies the borrowed descriptor before registration
replaces it. Failed decoding/validation can be retried; a completed
registration owns its code normally. User definitions replace pending and
decoded entries, and loading another image replaces old owners as before.
The generated instruction bytes and verification rules remain unchanged.

## Local evidence and limits

AppleClang 21, C++20, Release, image ON and IPO OFF: a compiler-evaluated
`sizeof(VMState)` probe reports 6,616 bytes before and 5,992 afterward,
a reduction of 624 bytes in this configuration. The probe uses an intentional
incomplete-template diagnostic and executes no production binary. This is
object size, not a process-memory, startup or throughput measurement.
The standalone ownership fixture separately reports 144 versus 168 bytes
for its own larger test value; that is not the production definition type.

The standalone fixture covers empty/pending/owned replacement, cleanup,
independent names, repeated destruction and failed staged construction;
it also passes ASan/UBSan. Image and source modes each pass 54 VM cases
(388,244 assertions) and their focused CLI/math/lifetime CTests. The actual
image-enabled decoder fault fixture walks 115 allocation points in verified
mode and 165 in full-check mode, requiring retry, user replacement, image
reload and cleanup. With automatic IPO enabled, nine focused CTests pass,
including the ordinary IPO lifetime fixture and the image fault fixture.
Actual image-fixture compile commands contain the IPO flags. Generated
math-library source/image bytes match the retained baseline.

These are local correctness and finite object-size observations. Native
Linux/Windows checks, other compiler/library ABIs, profile/native/threaded
modes, full VM sanitizers and whole-cohort elapsed/startup/resource gates
remain required before adoption. A different tag test or code layout may
cost more than the reduced storage saves. In particular, the earlier
Windows image-default sparse-array regression is preserved; the table's
size does not establish its cause or waive it. Windows still defaults to
the source loader.

No local production elapsed benchmark or CPU sampling is performed. Any
future timing study must be separately declared, preserve every original
outcome, retain the approved regression/Howard/startup rules and fit the
864,000 total recorded goal-second cap. PR #712's failures, inconclusive
arithmetic/startup observations and missing native artifact remain separate.

## Historical correctness publication in PR #713

PR #713's dedicated native workflow covers Linux, macOS and
Windows with image, source fallback, profile and native-function modes,
plus supported threaded modes and a Linux sanitizer build. It retains the
actual tested executables, generated library data, public source inputs,
compiler commands and CTest logs. A verifier checks the original uploaded
artifact against its recorded hashes; its synthetic fixture tests reject
missing, corrupt or misidentified bytes. This checks retention, not runtime
qualification. Partial builds retain available bytes without being accepted
as complete. Ordinary elapsed workflows exclude this exact topic branch;
correctness publication starts no replacement performance cohort.

The first published head `a601bee8` retains all 33 original native/support
archives and six core logs. Four strict native configurations and two core
configurations failed because the new lifetime fixture wrote an extension-only
one-line replacement definition. A local exact-compile diagnostic reproduces
the POSIX parser error and accepts the multiline POSIX definition. The fixture
now uses that portable spelling; VM runtime code is unchanged. Original failed
checks remain recorded, and this correction requires its own hosted checks.
