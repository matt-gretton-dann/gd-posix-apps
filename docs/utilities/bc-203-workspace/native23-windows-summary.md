# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.980 [0.8703176398744377, 1.0767327466112366] | inconclusive-noisy |
| full | aa | `loop_1000000` | 0.997 [0.9870765106567566, 1.0074786328351304] | pass |
| full | aa | `calls_200000` | 1.004 [0.9865068472759088, 1.0138511001755637] | pass |
| full | aa | `array_200000` | 1.003 [0.9794626238440776, 1.0121238191995856] | pass |
| full | aa | `compare_high_1000d_x100000` | 0.998 [0.9875132008899807, 1.0062619081672257] | pass |
| full | aa | `compare_low_1000d_x100000` | 0.996 [0.9334666777071912, 1.0125731466409575] | pass |
| full | aa | `compare_equal_1000d_x100000` | 0.995 [0.9890854808537218, 1.0059949240063726] | pass |
| full | aa | `fibonacci_1000_x100` | 1.006 [0.9903306957403861, 1.0120803073384426] | pass |
| full | aa | `multiply_100d_x100000` | 1.002 [0.9715569099998767, 1.063801900167721] | inconclusive-noisy |
| full | aa | `multiply_10000d_x1000` | 1.001 [0.9975311783971649, 1.0045785670679959] | pass |
| full | aa | `sqrt_scale100_x5000` | 0.996 [0.985053038850177, 1.010661169732108] | pass |
| full | aa | `multiply_dense_10000d_x500` | 1.000 [0.994335917790539, 1.0035775132894253] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.000 [0.9862952686214372, 1.0176197571392704] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.001 [0.9984010839117251, 1.007815714019484] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 0.999 [0.9631717455615565, 1.0141568485787662] | pass |
| full | aa | `power_100d_e17_x1000` | 1.003 [0.9945047314849214, 1.0107845181023531] | pass |
| full | aa | `sparse_array_200000` | 0.991 [0.9488358497898565, 1.0538710563856892] | inconclusive-noisy |
| full | aa | `decimal_input_4000d_x100` | 0.998 [0.9858733695827446, 1.0045138045044022] | pass |
| full | aa | `streaming_statements_20000` | 1.000 [0.9902578167975247, 1.0132987194478846] | pass |
| full | paired | `startup` | 1.066 [0.9593763509894947, 1.129650422931541] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.980 [0.9439526710714847, 1.0236545071128529] | pass |
| full | paired | `calls_200000` | 1.002 [0.9333463394712892, 1.0333367601437022] | pass |
| full | paired | `array_200000` | 0.999 [0.9609769719982778, 1.0415039125964867] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.976 [0.823781384096591, 1.0161637312142098] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.984 [0.9508660435360091, 0.9985826622070747] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.988 [0.8756488393443209, 1.0312084536668495] | pass |
| full | paired | `fibonacci_1000_x100` | 0.989 [0.9330011228347144, 1.0336147259009087] | pass |
| full | paired | `multiply_100d_x100000` | 0.995 [0.9455515775712833, 1.0322398543413198] | pass |
| full | paired | `multiply_10000d_x1000` | 0.877 [0.8648782406391987, 0.8875835465261374] | pass |
| full | paired | `sqrt_scale100_x5000` | 0.994 [0.9194732745347396, 1.0609689926439443] | inconclusive-noisy |
| full | paired | `multiply_dense_10000d_x500` | 0.840 [0.8237766591827018, 0.8494009020869353] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 1.001 [0.9974953158651749, 1.0320008031706467] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 0.331 [0.3297106670287932, 0.3350709154664319] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.983 [0.8703323199022713, 1.0199708949780235] | pass |
| full | paired | `power_100d_e17_x1000` | 1.001 [0.9898567233096889, 1.0533076326627249] | inconclusive-noisy |
| full | paired | `sparse_array_200000` | 0.994 [0.9500351389928045, 1.0057279545719866] | pass |
| full | paired | `decimal_input_4000d_x100` | 0.997 [0.9851061452933888, 1.000855431018209] | pass |
| full | paired | `streaming_statements_20000` | 0.953 [0.8355350356919586, 0.9926995460142197] | pass |
