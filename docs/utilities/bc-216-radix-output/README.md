# Native nondecimal output classes

The test-first corpus and representative-subset checks fail before the new
binary/octal output groups exist. Each full case emits a dense 1000-decimal-digit
integer 300 times in its requested output base. Separate groups ensure both
enter every native PR subset. Test oracles decode expected digits back to the
source integer. All 40 prior full programs, oracles and parameters are checked
unchanged; full corpus now has 42 cases.

Sixteen benchmark/CLI groups pass, including a native quick execution of every
case against accepted-output source 7424aac. Six native-policy orchestration
groups pass. The four separate full-size checks in correctness-smoke/ use the
actual frozen baseline 7424aac and grouped-output candidate ca00661, whose
complete source/compiler metadata is retained in checked.json. Both classes
pass exact output/status/stderr/deadline checks with both implementations.
These are correctness smokes, not paired timing evidence or acceptance of the
unmerged grouped-output runtime change. Historical reports retain old corpus
identities; new native results must name their actual harness source.
