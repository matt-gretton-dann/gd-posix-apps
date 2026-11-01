# Final-head sqrt boundary artifacts

Refs #349, #212, #200 and #197. PR #350 final head
`967b1c6522d7b1102f597c7414b13958146ac93e` has all fourteen check
runs successful. The final evidence-only commit triggered native CI again;
no favorable rerun of unchanged source was requested. The earlier runtime
source d252da1, initial confirmed-regression and rejected sqrt-boundary
datasets remain separately preserved. Do not replace or pool them.

Run 41324588691 retains digest/source-verified 1750 Linux, 1750 macOS and
1780 Windows checked invocations, 5280 total. No class meets the established
two-pass confirmed-regression rule. Windows equality remains slower/noisy
at 1.054 [1.04048,1.08060]; Linux equality is 1.032 [1.01937,1.05458].
MacOS intervals remain wide. These results do not establish equivalence or
a beneficial compiler mechanism; the comparison limitation stays in #200.
All controls, confirmation samples, compiler/binary identities and complete
class decisions remain in the unmodified archives.

PR #350 merged as 71b3683eae1d1bae72e7d062a9de5e87c144ff9a and
local main was immediately fast-forwarded. This separate documentation-only
archive preserves final-head observations without changing the source again.
