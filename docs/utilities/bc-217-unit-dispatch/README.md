# Unit arctangent dispatch screen

Local macOS screening evidence for #217 and PR #586, not native acceptance.
The previous head `0892c8f26590646f41b394984e5be4a4867f9038` improved
scale-20 unit calls but regressed Linux scale-100 unit calls by 13.65%
(paired interval [12.83%, 13.95%], artifact 13398784587). That adverse
report remains archived. This repair handles positive units before general
sign setup and negative units inside the sign-normalization branch.

`screen.py` records 17 randomized paired rounds after two warm-ups. The
baseline is `fa229de80dfc939468e06efec8e0a935eca5ba87`; the candidate embeds
the source whose SHA-256 appears in `samples.json`. Both binaries use the
same retained-result VM parent. The A/A role reuses the baseline binary;
it is not an independent rebuild. Payloads, all measured nanoseconds,
platform identity and binary hashes are in `samples.json`. `summary.txt`
contains medians and bootstrap intervals; `sha256.json` hashes those files
and the script. Paths in the script identify the original local builds.

Candidate/baseline medians are 0.8613 (unit scale 20), 0.9330 (unit scale
100), 0.9616 (negative unit scale 20), 0.4799 (near one) and 0.9961
(reciprocal control). Runs last roughly 0.13–0.28 seconds. These results
support further testing, but do not establish cross-platform qualification,
cold-start behavior or resource use. Hosted full and holdout reports and
their independent A/A controls remain required before merging.

Correctness: all 26 math CLI tests pass with the freshly built candidate
and portable Howard bc 7.0.3 source mode. The decision fixture independently
regenerates with 621 certified results and 201/212 observed outcomes; the
remaining outcomes retain their modeled guard or infeasibility explanations.
The embedded text matches the source and its preparsed image regenerates
byte for byte. Tests cover both signs, nondecimal input bases, scales around
the literal boundary and a caller's preexisting sign-variable binding.
