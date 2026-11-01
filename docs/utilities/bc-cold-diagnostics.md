# Cold diagnostic separation

Refs #202 and #197. This candidate asks GCC/Clang to place arithmetic and VM
fatal formatting off the success path and avoid inlining it. MSVC receives its
noinline hint; other compilers retain the ordinary noreturn definition. Entry
validation, runtime semantic errors, diagnostic arguments, streams, source
locations and exit status remain active and unchanged. There are no unchecked
assumptions or new runtime/compiler requirements.

This is an experiment until actual code size, all-class matched release timing
and native evidence support a decision. Fewer formatting bodies or ownership
operations alone do not establish a runtime improvement.
