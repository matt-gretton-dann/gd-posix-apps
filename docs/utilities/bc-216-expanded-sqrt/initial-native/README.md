# Initial expanded sqrt native failure

Refs #216, #212 and #197. Exact source d6eb63c941d9cd66ca45cac384efde9ba6864a9d,
base 704d8013c2ff877357bf9dc42a0ae68b571c6c3e, workflow 41326973722.
Windows Howard 7.0.3 exceeds the unchanged 30-second deadline on
`sqrt_tiny_scale4096_x10`. The matrix is incomplete and not accepted.
The digest-verified archive retains 800 checked A/A rows and 78 completed
paired rows, plus both explicit hard-failure records. No performance pass
is inferred from those partial observations.

Reduce this case to one repeat, preserving scale 4096, its radicand, exact
oracle and the existing timeout. This is a changed workload, not an
unchanged-source favorable rerun. Initial local ten-repeat results remain
labeled with their actual source and program hashes. Other native runs
from this initial source, if incomplete/cancelled, are not acceptance data.
