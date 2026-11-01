# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.927 [0.7884365480004252, 1.2254691767370542] | inconclusive-noisy |
| full | aa | `loop_1000000` | 0.998 [0.9776943717924709, 1.0057177629184408] | pass |
| full | aa | `calls_200000` | 1.000 [0.9760363707240325, 1.0238396985366809] | pass |
| full | aa | `array_200000` | 1.001 [0.9923999787295759, 1.0265116613402756] | pass |
| full | aa | `compare_high_1000d_x100000` | 0.985 [0.9692749878378952, 1.0084121465822158] | pass |
| full | aa | `compare_low_1000d_x100000` | 1.009 [0.989902319383731, 1.0280507399721823] | pass |
| full | aa | `compare_equal_1000d_x100000` | 0.991 [0.9794451324516985, 1.0425316248332117] | pass |
| full | aa | `fibonacci_1000_x100` | 1.008 [0.9853912845847107, 1.0152644490462241] | pass |
| full | aa | `multiply_100d_x100000` | 0.997 [0.9927084577689163, 1.0105950955748884] | pass |
| full | aa | `multiply_10000d_x1000` | 1.001 [0.991053168716217, 1.0114874790268356] | pass |
| full | aa | `sqrt_scale100_x5000` | 1.002 [0.9994926663301315, 1.0063977182809856] | pass |
| full | aa | `multiply_dense_10000d_x500` | 0.998 [0.9896135411518416, 1.0034148234322162] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.000 [0.9978468275730498, 1.00290857062046] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.000 [0.9993285141831802, 1.0004421583329737] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 0.998 [0.971764239448718, 1.0202916797767267] | pass |
| full | aa | `power_100d_e17_x1000` | 1.000 [0.994504775593976, 1.004663701667094] | pass |
| full | aa | `sparse_array_200000` | 1.014 [0.9983335128553364, 1.0231765429085538] | pass |
| full | aa | `decimal_input_4000d_x100` | 0.999 [0.9965003667064991, 1.0003854891293422] | pass |
| full | aa | `streaming_statements_20000` | 1.004 [0.988796309076089, 1.0284492744496465] | pass |
| full | paired | `startup` | 1.166 [0.9077989043211837, 1.2625979957176934] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.764 [0.7511001391557389, 0.7971024740214362] | pass |
| full | paired | `calls_200000` | 0.829 [0.8228015281079875, 0.8398106796258715] | pass |
| full | paired | `array_200000` | 0.777 [0.766974613258907, 0.7890759371254543] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.775 [0.7713018255415139, 0.7889487185116562] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.978 [0.9634513791757295, 1.0041864803485399] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.970 [0.9647717786933528, 0.9796686882536915] | pass |
| full | paired | `fibonacci_1000_x100` | 0.926 [0.9007050684648429, 0.9620948165372372] | pass |
| full | paired | `multiply_100d_x100000` | 0.930 [0.9218406270087899, 0.9334482518699811] | pass |
| full | paired | `multiply_10000d_x1000` | 1.028 [1.0256771285990103, 1.03250334799838] | pass |
| full | paired | `sqrt_scale100_x5000` | 1.115 [1.09911619475097, 1.1224194080132412] | confirmed regression |
| full | paired | `multiply_dense_10000d_x500` | 1.011 [1.0105159442912561, 1.012349279002619] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.964 [0.9600688493261422, 0.9700106514129432] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 1.122 [1.1202541734341676, 1.1248190247082632] | confirmed regression |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.998 [0.9872588764038652, 1.0342642460529652] | pass |
| full | paired | `power_100d_e17_x1000` | 0.965 [0.9629086101170792, 0.9726296795257922] | pass |
| full | paired | `sparse_array_200000` | 0.775 [0.7655934561629654, 0.8125059976262031] | pass |
| full | paired | `decimal_input_4000d_x100` | 1.002 [0.9997456771035023, 1.00278634852072] | pass |
| full | paired | `streaming_statements_20000` | 0.991 [0.9770309878839937, 0.9944634927431641] | pass |
