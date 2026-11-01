# CodeQL narrow-bound annotation

The aggregate CodeQL check fails for source 225bfaf at the radix-power loop.
Small traits promote `(base_ - 1) / obase` to a wider integer while the loop
variable is NumType. The annotation is retained verbatim in structured form.

For supported traits, storage radix B fits NumType, output base b is 2–16,
and the loop only multiplies p by b when p <= floor((B-1)/b). Therefore the
product is <= B-1 and remains representable. Current small-trait exact-oracle
and sanitizer tests pass before the change; no supported-input overflow is
reproduced. The implicit promotion obscures that proof to static analysis.

Make the bound an explicitly representable NumType value before comparing.
This retains the mathematical guard and all supported behavior, without
suppressing CodeQL or weakening tests. The failing CodeQL check is the
regression to clear; fresh checks are required before merging. Earlier native
speed observations retain their actual original source.
