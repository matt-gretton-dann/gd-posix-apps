# Windows startup launch-path diagnostic

The #728 and #729 Windows startup studies observed persistent differences
between independently built controls. In #729 the two builds have identical
PE executable sections. Data sections and full executable identities differ.
The source of the startup difference remains unresolved.

This #200 diagnostic builds accepted runtime
`19ef80161fd2bdb24bf9ba7e93942caa97ea6ea1` twice with native x64 MSVC
on one hosted `windows-2025` runner. It deploys both executables as
`bc.exe` in equal-length sibling paths. Each adjacent pair of blocks
uses both build-to-path assignments in a fixed seeded order. Both files
are recopied before every block. The existing startup collector checks the
exact executable hash and output before accepting each observation. Each
block includes the base executable, an alias that launches that exact path,
and the independent executable at the other path.

The schedule fixes 20 blocks, two stages per block, five warmups and 151
paired rounds per stage. It uses 2,000 resamples with the existing paired
interval calculation, a five-second child limit, and a 1,800-second total
collection limit. The artifact preserves every checked observation, all
assignments, build and collector provenance, both independently built
executables and available failure evidence.

Review persistent same-direction intervals from both stages by assignment.
A build-linked pattern keeps its direction in both assignments. A path-linked
pattern reverses direction when the executable moves. Check the same-path
alias separately, and report counts and magnitudes for each orientation.
This is a descriptive crossover on one host. Cache effects, antivirus,
filesystem identity, build data and time-varying load can interact with
deployment, so the results cannot establish a specific cause or a portable
null false-alarm rate. This study grants no qualification or merge eligibility.
The original #728 and #729 evidence retains its own decisions.
