# Initial decimal-input component regression — not accepted

Source `dd88b5cec60fc0796bea11998f547084db11f0fd` versus accepted parent
`a5c1cb1ad4051616afdce115b02ecc0247ad62f7` passes all 360 exact component
result/checksum/status checks, but one-digit conversion is 1.551 [1.548, 1.566]
of parent time. Nine paired measured internal intervals follow one warm-up.
Larger conversion gains do not supersede this losing class. Every control,
conversion-in and conversion-out row remains in the archive; no subtraction of
control time or pure-kernel attribution is made. No local builds/tests/profiles
overlap timing. This is a shared workstation, not a cross-platform guarantee.

The next revision must recover scalar construction while preserving the
language's base-independent hexadecimal single-digit rule. The existing
automated component workload reproduced this regression before the fix; its
raw data and exact source/compiler identities are retained rather than adding
a brittle wall-time assertion to ordinary correctness CI.
