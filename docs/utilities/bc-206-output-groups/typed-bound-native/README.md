# Typed-bound source remains held for Linux division

Source 9e47f595e433a377db99b2a197e14287456bc8a5 clears the CodeQL loop-type
annotation. A separate duplicate push job fails package setup with an external
HTTP 408; retry workflow 41289911113 attempt 2 passes. The original failure
remains historical and is not a source-analysis failure.

Native run 41289914497 compares exact base 422cbe90 with this source. Every
output/status/deadline check passes: Linux 1610, macOS/Windows 1550 each.
Linux confirms divide_scale100_x100000 at 1.062 [1.053,1.072]. macOS/Windows
have no confirmed losses. All digests/source identities and complete raw
samples, A/A and bounded confirmations remain in the ZIPs. Keep PR #337 held.

The previous source 225bfaf and its earlier native results retain their own
identities. A GCC-only out-of-line formatting wrapper is a bounded next
experiment; it does not establish an inlining, layout or hardware cause.
Do not replace or relabel these observations with a favorable later source.
