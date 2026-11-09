# Prospective storage-transition arithmetic cases

Declared in [issue705 before generation](https://github.com/matt-gretton-dann/gd-posix-apps/issues/705#issuecomment-6738238949).
These append to every original66-case arithmetic corpus, giving69 cases per
preset. `original-arithmetic-corpus.json` preserves all132 original full/held
names, groups, parameters and program hashes. No original RNG draws change.

The new generator uses its own `Random(corpus_seed XOR705197)` stream. Initial
values begin with1 and have independently drawn remaining decimal digits.
Full operand seed is197200; held-out is197201. The checksum modulus is1000000007.
All operations are ordinary bc statements, with no production name recognition.

| Class | Initial decimal digits full / held | Iterations full / held |
| --- | --- | --- |
| `storage_inline_prefix` |17 /18 |200000 /175000 |
| `storage_heap_snapshots` |180 /207 |50000 /43750 |
| `storage_promotion_churn` |27 /26 |100000 /87500 |

Inline-prefix work computes `b=a+i`, `c=b*123456789`, accumulates c modulo the
checksum modulus, then overwrites b with i. On native nine-place limbs, the
values cover two/three limbs and a one-limb overwrite. Its checksum is
`123456789*(N*a+N*(N-1)/2) mod M`.

Heap-snapshot work copies b=a and c=b, increments a, then accumulates the old c.
The checksum is `(N*a_initial+N*(N-1)/2) mod M`. Both snapshots must survive the
mutation of their shared initial value.

Promotion work uses B=10^27: `b=a+(i%97)`, `c=b+B`, `d=c`, `c=c-B`, accumulates
d+c, then overwrites c with i. This crosses three/four native limbs, preserves
a heap snapshot across detachment, and returns a slot to inline storage.
With q=N//97 and r=N%97, its checksum is
`(N*(2*a+B)+2*(q*97*96/2+r*(r-1)/2)) mod M`.

Quick fixtures keep these operand sizes and divide full iterations by100;
they are functional validation, never qualification. Tests independently
execute the integer checksum loops for all three presets and preserve every
original full/held program identity. Separate functional checks run only the
three quick programs on retained reference source20920, without collecting
elapsed values; this does not claim that the new705 runtime was tested there.

All three full/held classes are included in matched resource observations.
Their input generation uses exactly the elapsed corpus seeds, including197201
for held-out. Resource-only lifetime/cache probes remain separate. Every
neutral, adverse and inconclusive outcome must remain visible in the prospective
immutable full native study; no performance result is implied by these cases.
