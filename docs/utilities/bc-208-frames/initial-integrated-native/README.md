# Integrated frame candidate — Windows regression

Candidate d76e408e069a4824eca7ab1fc3fa35100a14a070 was measured against
accepted main 00d5896be1768d1936a075ccd47e4b529a914dd2 in native run
41278153784. This event preceded PR retargeting, so the comparison includes
both decimal conversion and frames; it is not an isolated frame comparison.
The separate local adjacent matrix retains the isolated observations.

Every output/status/deadline check passes: Linux/macOS 1450 invocations each,
Windows 1510 including bounded confirmations. Windows low-digit comparison
confirms a regression, ratio 1.114 [1.091,1.147]. Green advisory workflows do
not supersede this loss. All raw ZIPs, source identities and digests remain.
The candidate stays on hold while a changed MSVC candidate is evaluated.

The native Windows result does not establish a hardware or register-allocation
cause. Isolating the limb comparator from surrounding template/VM inlining is
a bounded next experiment. Other compilers must retain the generic policy.
