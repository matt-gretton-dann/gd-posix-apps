# Prospectively declared scalar-frame workloads

Refs #707 comment 6742027444, published before generation or enabled elapsed
observation. Keep the 66 accepted cases per full/held preset and all 132
historical program/parameter identities. Append these three frame classes.
Historical cases are exposed controls, not newly untouched holdouts.

Use `Random(corpus_seed XOR 707197)`, independently of the old RNG stream.
Full seed is 197200; held seed is 197201. A dense decimal has leading literal
1 followed by independently drawn decimal digits. Draws occur in the order
below. All checksums use modulus M=1000000007. Quick keeps full operand/depth
dimensions and divides full repetitions by 100. Counts are fixed prospectively.

| Class | Full | Held |
| --- | --- | --- |
| Wide locals | 180-digit globals, 17-digit arguments, 75000 calls | 207-digit globals, 18-digit arguments, 65625 calls |
| Dynamic chain | 180-digit globals, 17-digit arguments, 100000 calls | 207-digit globals, 18-digit arguments, 87500 calls |
| Recursive snapshots | 180-digit arguments, depth 37, 1200 calls | 207-digit arguments, depth 41, 1050 calls |

Wide locals draws global G, initial A and sixteen coefficients c_j uniformly
from 1..97. Function f(n) has autos a,b,c,d,e,h,j,k,l,m,o,p,q,r,t,u. It sets
auto_j=n+c_j and returns sum(j*auto_j), j=1..16. Initial global auto_j=G+j.
For i=0..N-1, accumulate f(A+i) modulo M. The closed checksum is
`[136*(N*A+N*(N-1)/2)+N*sum(j*c_j)] mod M`. Print all sixteen selected globals
modulo M afterwards; each must remain `(G+j) mod M`.

Dynamic chain next draws dense G and A, then c,d,y0 uniformly from 1..97. Globals are
x=G,t=G+1,u=G+2,z=31,y=y0. Function g(z) adds z to current dynamic x,
increments y and returns x. Function f(x) has autos t,u and returns t+u+y
after t=g(c), u=g(d). Accumulating f(A+i) gives
`[N*(2*A+2*c+d+y0+2)+2*N*(N-1)] mod M`. Final y is y0+2*N; x,t,u,z must keep
their original global values. Print large globals modulo M.

Recursive snapshots next draws dense A, then delta uniformly from 1..97. Globals
x=A,t=A+1,n=29. Function f(n,x) has auto t; return x when n==0, otherwise set
t=x+delta and return f(n-1,t)+x. Accumulating f(D,A+i) gives
`[(D+1)*(N*A+N*(N-1)/2)+N*delta*D*(D+1)/2] mod M`. Globals x,t,n must survive.
The parent reads its heap coefficient after the nested call returns.

The pure fixture evaluates finite integer loops and explicit recursive-value
stacks independently of these closed formulas for all presets, verifies RNG
draws/parameters, and checks historical identities. Functional source checks
do not collect elapsed fields or establish candidate performance. Ordinary and
profile resource inputs must use exactly the elapsed program/oracle bytes and
operand seed. Extra lifetime probes remain separately labeled diagnostics.

Success here cannot substitute for original-case losses or inconclusive
controls. All original arithmetic, full/held math, startup and resource gates,
including the unchanged regression cap and Howard exception, remain required.
