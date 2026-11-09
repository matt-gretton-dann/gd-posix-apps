# Reusable scalar-local bindings experiment

Refs #707, #207 and #197. Default OFF; no performance acceptance is claimed.

The accepted VM moves each declared scalar into a mixed saved-local buffer,
installs a zero, then moves the saved Number back on return. The experiment
keeps caller values in place and binds local letters to stable values in a
reusable depth frame. Unselected letters inherit the current dynamic binding.
Nested frames restore pointers on return or exception; local destruction releases
owners, while vectors retain capacity. Array locals and special-variable saving
keep the accepted path. No math-specific opcode, source recognition, restricted
ownership contract or architecture assumption is introduced.

`ENABLE_BC_SCALAR_BINDINGS=OFF` retains the original implementation. Enabled
scalar reads/writes add a pointer indirection. Each active frame retains local
values plus prior pointers; capacity, pointer maps and stack metadata may cost
more memory than the saved-variant path. Reused capacity is not a claim of reduced
RSS, and zero Number transfers during binding is not elapsed-time evidence.
A frame with no selected scalar names changes nothing and allocates nothing.

The standalone contract test checks dynamic inheritance, nested writes,
128-level stable references, ownership/snapshot lifetime, empty selection,
invalid and duplicate selectors, constructor/allocation failure and reuse with
no retained values or save/restore transfers. The parsed VM oracle also covers
argument evaluation before rebinding, redefinition, recursion and outer globals,
in full and verified execution. Existing array, return/COW, special-register,
fault-injection and signal tests remain required for enabled builds.

Native correctness workflow runs Linux, macOS and Windows ordinary/profile/OFF
configurations and Linux Clang ASan/UBSan. It retains exact VM compile selection,
source/cache/CDFs, actual binaries and correctness outputs. This is preflight,
not elapsed qualification. Any enabled adoption requires a separately declared
immutable source, fresh held-out workloads and complete original native
arithmetic/full-math/held-math, startup and resource evidence. Do not reuse old
sources' observations as candidate results or retry for favorable data.

No local production elapsed benchmarks or native CPU sampling. Preserve the
864000 total goal-second cap and 20GiB free disk floor; unmet parent issues
remain open at the cap. Initial performance and memory benefit are unknown.
