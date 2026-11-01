# Cold diagnostic evaluation

Refs #202 and #197. The cold/noinline formatter candidate is rejected: local
timings are broadly neutral, executable file and page-rounded text sizes are
unchanged, and sampled RSS shows no general improvement. Keep ordinary noreturn
definitions and existing runtime diagnostics.

[The complete report](bc-202-cold-diagnostics/README.md) preserves exact source
identities, the portable candidate patch, local/native timing, process memory,
code-size details, generic-fallback checks and the #202 completion decisions.
No claim of universal neutrality or cross-platform memory reduction is made.
