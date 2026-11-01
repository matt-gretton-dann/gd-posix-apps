# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.941 [0.9061751320652592, 1.0494459992817537] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.041 [0.8165771939050658, 1.1077823502492725] | inconclusive-noisy |
| full | aa | `calls_200000` | 0.965 [0.8773065963884221, 1.1725258399201794] | inconclusive-noisy |
| full | aa | `array_200000` | 0.985 [0.8256611438806127, 1.0531412990692428] | inconclusive-noisy |
| full | aa | `compare_high_1000d_x100000` | 0.969 [0.9113762533320203, 1.1776172919665522] | inconclusive-noisy |
| full | aa | `compare_low_1000d_x100000` | 1.014 [0.851744781296871, 1.0804627148139065] | inconclusive-noisy |
| full | aa | `compare_equal_1000d_x100000` | 0.969 [0.8432913431108782, 1.1703829093519709] | inconclusive-noisy |
| full | aa | `fibonacci_1000_x100` | 1.004 [0.7044338176022771, 1.1020461413573936] | inconclusive-noisy |
| full | aa | `multiply_100d_x100000` | 1.064 [0.969646142920303, 1.2199660317642598] | inconclusive-noisy |
| full | aa | `multiply_10000d_x1000` | 0.976 [0.960786228058663, 1.008601320345083] | pass |
| full | aa | `sqrt_scale100_x5000` | 0.976 [0.9413840037625424, 1.1401080230446827] | inconclusive-noisy |
| full | aa | `multiply_dense_10000d_x500` | 0.998 [0.9359806364617905, 1.0680824699140012] | inconclusive-noisy |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.038 [0.9654704664640209, 1.114675651643189] | inconclusive-noisy |
| full | aa | `multiply_sparse_10000d_x1000` | 1.016 [0.9094004920408248, 1.0693102616758732] | inconclusive-noisy |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 1.052 [0.925167612185927, 1.2651101584560966] | inconclusive-noisy |
| full | aa | `power_100d_e17_x1000` | 1.020 [0.8838179540187502, 1.1426864889649995] | inconclusive-noisy |
| full | aa | `sparse_array_200000` | 0.941 [0.9179978742825944, 1.1223240213267427] | inconclusive-noisy |
| full | aa | `decimal_input_4000d_x100` | 1.000 [0.9084753113970694, 1.0581737779910898] | inconclusive-noisy |
| full | aa | `streaming_statements_20000` | 0.998 [0.9102471920367934, 1.1664771706037573] | inconclusive-noisy |
| full | paired | `startup` | 1.022 [0.8897112100490931, 1.2741799411256736] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.959 [0.8456590800211898, 1.0749252850533344] | inconclusive-noisy |
| full | paired | `calls_200000` | 0.983 [0.8518388675974177, 1.1877135327717818] | inconclusive-noisy |
| full | paired | `array_200000` | 0.956 [0.8860475888218726, 1.1377122590428714] | inconclusive-noisy |
| full | paired | `compare_high_1000d_x100000` | 1.012 [0.8688600529754019, 1.3117507499761483] | inconclusive-noisy |
| full | paired | `compare_low_1000d_x100000` | 0.986 [0.9347679696162292, 1.1192461797913238] | inconclusive-noisy |
| full | paired | `compare_equal_1000d_x100000` | 1.020 [0.8829421883347466, 1.1893578287357025] | inconclusive-noisy |
| full | paired | `fibonacci_1000_x100` | 0.982 [0.8362691418487602, 1.3881380478935117] | inconclusive-noisy |
| full | paired | `multiply_100d_x100000` | 1.016 [0.9331968429228776, 1.2269859492772461] | inconclusive-noisy |
| full | paired | `multiply_10000d_x1000` | 0.846 [0.8207875037087113, 0.8972473478781617] | pass |
| full | paired | `sqrt_scale100_x5000` | 0.996 [0.9495852323479044, 1.142137745248359] | inconclusive-noisy |
| full | paired | `multiply_dense_10000d_x500` | 0.879 [0.7974301302862946, 1.0041185128262924] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.960 [0.735749313268006, 1.0789614815540272] | inconclusive-noisy |
| full | paired | `multiply_sparse_10000d_x1000` | 0.334 [0.32682931211941924, 0.3589697278341128] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.950 [0.7273363800573638, 1.0845253963512351] | inconclusive-noisy |
| full | paired | `power_100d_e17_x1000` | 0.993 [0.9527690608481589, 1.354059943641207] | inconclusive-noisy |
| full | paired | `sparse_array_200000` | 0.949 [0.8983998550027366, 1.0307410778505963] | pass |
| full | paired | `decimal_input_4000d_x100` | 1.054 [0.9114552249733835, 1.1490214858109664] | inconclusive-noisy |
| full | paired | `streaming_statements_20000` | 0.985 [0.9193596195319325, 1.0594393950111127] | inconclusive-noisy |
