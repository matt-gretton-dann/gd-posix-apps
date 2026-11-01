# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.809 [0.6855205400271859, 1.2224454925510393] | inconclusive-noisy |
| full | aa | `loop_1000000` | 0.998 [0.9900581401364854, 1.0150950203162998] | pass |
| full | aa | `calls_200000` | 1.000 [0.995832613803081, 1.0168544437535665] | pass |
| full | aa | `array_200000` | 0.988 [0.9712714225087072, 1.0122468110811222] | pass |
| full | aa | `compare_high_1000d_x100000` | 1.006 [0.975128781392816, 1.028805168923271] | pass |
| full | aa | `compare_low_1000d_x100000` | 1.004 [0.9856493385336212, 1.0625858160181787] | inconclusive-noisy |
| full | aa | `compare_equal_1000d_x100000` | 0.992 [0.9858275523131517, 1.020219267600013] | pass |
| full | aa | `fibonacci_1000_x100` | 1.006 [1.0004612960465933, 1.0333189625211956] | pass |
| full | aa | `multiply_100d_x100000` | 1.010 [0.9970542810564227, 1.0318107871356867] | pass |
| full | aa | `multiply_10000d_x1000` | 1.000 [0.9907067474549454, 1.0484484217123256] | pass |
| full | aa | `sqrt_scale100_x5000` | 1.003 [0.9934327002768203, 1.006310319597898] | pass |
| full | aa | `multiply_dense_10000d_x500` | 0.999 [0.9970845286895595, 1.002299931930073] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 0.999 [0.994224654818135, 1.000505242792155] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.000 [0.9649239383914024, 1.008118959217695] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 1.000 [0.9566444457538857, 1.025176330638349] | pass |
| full | aa | `power_100d_e17_x1000` | 1.001 [0.9906380659355966, 1.012088873194572] | pass |
| full | aa | `sparse_array_200000` | 0.996 [0.9849485648892241, 1.0022483509354134] | pass |
| full | aa | `decimal_input_4000d_x100` | 0.999 [0.9951985152313043, 1.0213448958424896] | pass |
| full | aa | `streaming_statements_20000` | 0.997 [0.9945274430274981, 1.020611388296738] | pass |
| full | paired | `startup` | 1.217 [0.7931672052264858, 1.3085542966352375] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.999 [0.9802185541673452, 1.0161103834654572] | pass |
| full | paired | `calls_200000` | 1.007 [0.9811842605749855, 1.0168991377069097] | pass |
| full | paired | `array_200000` | 1.021 [0.9888889407686299, 1.0324611092512084] | pass |
| full | paired | `compare_high_1000d_x100000` | 1.004 [0.9599391240406832, 1.0531339617900342] | inconclusive-noisy |
| full | paired | `compare_low_1000d_x100000` | 0.993 [0.970915461367909, 1.006266707712865] | pass |
| full | paired | `compare_equal_1000d_x100000` | 1.003 [0.9769000973566618, 1.0473028414164056] | pass |
| full | paired | `fibonacci_1000_x100` | 1.036 [1.0227931707966436, 1.0608555365956664] | inconclusive-noisy |
| full | paired | `multiply_100d_x100000` | 1.005 [0.9892732945573318, 1.0455937152310275] | pass |
| full | paired | `multiply_10000d_x1000` | 0.897 [0.8905513958932407, 0.9044697612407133] | pass |
| full | paired | `sqrt_scale100_x5000` | 0.999 [0.9947805159014803, 1.0181134845304425] | pass |
| full | paired | `multiply_dense_10000d_x500` | 0.895 [0.8930964605765472, 0.898770898059487] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.999 [0.9945735114143607, 1.0014103335650804] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 0.363 [0.3598204772693405, 0.3633234776669717] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.995 [0.9647701231536812, 1.02268791746983] | pass |
| full | paired | `power_100d_e17_x1000` | 0.999 [0.9849080704714576, 1.0016554762039682] | pass |
| full | paired | `sparse_array_200000` | 0.995 [0.981133827999898, 1.0063250694323127] | pass |
| full | paired | `decimal_input_4000d_x100` | 0.999 [0.9985371226341874, 1.0273221799386694] | pass |
| full | paired | `streaming_statements_20000` | 0.990 [0.9848253634417871, 0.9936706515800637] | pass |
