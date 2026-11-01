# GCC formatting-isolation candidate

The typed-bound predecessor's Linux scalar-division loss remains in
../typed-bound-native/. Source before this policy is 9e47f59; the changed
candidate marks only the BasicNumber formatting wrapper noinline under GCC,
retaining generic compiler policy and the same body elsewhere.

The hypothesis is to isolate formatting from surrounding execution-frame
inlining. Neither the observed division regression nor this change establishes
its hardware or compiler cause. All 151 local arithmetic/VM cases (507192
assertions) and seven component groups pass. The matched Mac executable
identity is recorded separately, without relabelling original native data.
Fresh CodeQL and native evidence are required before accepting the candidate.
