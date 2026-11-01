# Native-matrix and array integration

The runtime candidate now includes the #216 binary/octal workload prerequisite
and accepted array storage, rather than relying on the older component timing
context. All 193 Release cases / 691439 assertions and eight CLI/tool groups
pass. OFF ASan/UBSan checks pass 26 VM/array/radix-output cases / 71936 assertions,
15 CLI checks and six dual-mode offsets. Profiling passes 20 radix/array/mode
assertions in five cases plus eleven CLI groups.

The parent report retains its actual baseline 7424aac and component candidate
ca00661. Those timing rows are not relabelled as this later integration. New
native runs must identify the full candidate and its exact matrix predecessor.
