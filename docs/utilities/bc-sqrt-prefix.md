
The prefix cutoff uses working **root-coefficient digits**, rather than the
requested output scale. For `sqrt(2)` with the native nine-digit radix and ten
guard places, the root coefficient has `S+11` decimal digits. Requested scales
116, 117 and 118 therefore exercise 127, 128 and 129 working digits. The
profiling fixture computes these scales from the reported radix, independently
checks the exact output floor inequalities and result scale, and observes the
selected path on both sides of the cutoff. Earlier elapsed cases named
`crossover_120/124/128/132` are retained with their original names and programs;
they measure nearby sizes but do not establish that they straddle this cutoff.
