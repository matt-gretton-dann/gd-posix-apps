# GCC outlining candidate remains on hold

Source 254073f445346c1842929e51269eb5d101c7c91f, native run 41283133653,
compares exact predecessor 8e6c1495cdeb8c797f1161750634732fd71b348b.
All 1480 Linux and 1450 macOS/Windows invocations pass exact output, status
and deadline checks. Digests and exact sources are in the adjacent audits.

Linux still confirms ibase_literal_flip_20000 at 1.072 [1.060,1.090].
The other five originally confirmed losses are not confirmed in this run;
that does not establish their cause. Neither macOS nor Windows confirms a
loss. Advisory green checks do not authorize adoption of this candidate.
Keep PR #329 on hold. These are changed-source observations; original losses
remain archived in initial-integrated-native. No favorable subset replaces
the full raw samples or the remaining Linux regression.
