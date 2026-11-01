# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 1.007 [0.8807698689804265, 1.087915259901954] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.026 [1.0040988039724204, 1.128601700047556] | inconclusive-noisy |
| full | aa | `calls_200000` | 1.009 [0.9139897080231286, 1.1450511462160922] | inconclusive-noisy |
| full | aa | `array_200000` | 1.012 [0.8705293768978584, 1.1452552094162036] | inconclusive-noisy |
| full | aa | `compare_high_1000d_x100000` | 1.132 [1.013509517788987, 1.2808133502080334] | inconclusive-noisy |
| full | aa | `compare_low_1000d_x100000` | 1.001 [0.8707347650650882, 1.0431966849964105] | pass |
| full | aa | `compare_equal_1000d_x100000` | 1.125 [0.9963745720885311, 1.20556225103116] | inconclusive-noisy |
| full | aa | `fibonacci_1000_x100` | 0.925 [0.7510975520672613, 1.0329388882914183] | pass |
| full | aa | `multiply_100d_x100000` | 1.045 [1.0130747961783149, 1.1975070816147682] | inconclusive-noisy |
| full | aa | `multiply_10000d_x1000` | 0.981 [0.9335693613279554, 1.071690667903998] | inconclusive-noisy |
| full | aa | `sqrt_scale100_x5000` | 1.003 [0.9765194820510066, 1.1118454559997892] | inconclusive-noisy |
| full | aa | `multiply_dense_10000d_x500` | 1.001 [0.9476067078375666, 1.0429158659886109] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.005 [0.9406489827012001, 1.0603224706698926] | inconclusive-noisy |
| full | aa | `multiply_sparse_10000d_x1000` | 0.998 [0.9466054203691927, 1.0479721259745465] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 1.008 [0.8182793883732736, 1.0765555074509081] | inconclusive-noisy |
| full | aa | `power_100d_e17_x1000` | 0.996 [0.8699643988082303, 1.0065570588359622] | pass |
| full | aa | `sparse_array_200000` | 1.038 [0.9769643977165339, 1.1122484126828116] | inconclusive-noisy |
| full | aa | `decimal_input_4000d_x100` | 0.963 [0.9098702398902706, 1.0268782919305584] | pass |
| full | aa | `streaming_statements_20000` | 1.054 [0.998461006616362, 1.257962833067708] | inconclusive-noisy |
| full | paired | `startup` | 1.107 [0.9424469537252951, 1.181230917561709] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.973 [0.9030827123023171, 1.0373369450829772] | pass |
| full | paired | `calls_200000` | 0.950 [0.8628495349693937, 1.1580676101917944] | inconclusive-noisy |
| full | paired | `array_200000` | 1.020 [0.9261755628593468, 1.2257156128953612] | inconclusive-noisy |
| full | paired | `compare_high_1000d_x100000` | 1.006 [0.9395490211479134, 1.1171880476953282] | inconclusive-noisy |
| full | paired | `compare_low_1000d_x100000` | 1.008 [0.8291926323328817, 1.0151456114137214] | pass |
| full | paired | `compare_equal_1000d_x100000` | 1.039 [0.9879186042342162, 1.5127623988139247] | inconclusive-noisy |
| full | paired | `fibonacci_1000_x100` | 0.994 [0.8047683565464969, 1.117917171208642] | inconclusive-noisy |
| full | paired | `multiply_100d_x100000` | 0.978 [0.7990101992043325, 1.0633984959334624] | inconclusive-noisy |
| full | paired | `multiply_10000d_x1000` | 0.823 [0.7708343095355934, 0.9033087502229705] | pass |
| full | paired | `sqrt_scale100_x5000` | 0.992 [0.8458500174485535, 1.4336936531161466] | inconclusive-noisy |
| full | paired | `multiply_dense_10000d_x500` | 0.851 [0.8360194330037343, 0.9252975625817335] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.977 [0.9267653746416256, 0.9967775791180412] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 0.347 [0.3277236138548825, 0.3585411064736333] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.975 [0.8516344182171377, 1.2548027911299129] | inconclusive-noisy |
| full | paired | `power_100d_e17_x1000` | 0.986 [0.7627788496494519, 1.029558967991462] | pass |
| full | paired | `sparse_array_200000` | 0.999 [0.8986070056482502, 1.096438668629175] | inconclusive-noisy |
| full | paired | `decimal_input_4000d_x100` | 1.032 [0.967369456714517, 1.0539768886645733] | inconclusive-noisy |
| full | paired | `streaming_statements_20000` | 1.139 [0.9829710672334008, 1.2083852339296013] | inconclusive-noisy |
