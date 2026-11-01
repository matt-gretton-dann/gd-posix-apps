# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 1.006 [0.8922687449533268, 1.0785157821040678] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.008 [0.9775447253524359, 1.016267239513421] | pass |
| full | aa | `calls_200000` | 1.003 [0.9850256716945233, 1.03093171745617] | pass |
| full | aa | `array_200000` | 1.006 [0.9787796725695207, 1.0104847842202476] | pass |
| full | aa | `compare_high_1000d_x100000` | 0.999 [0.9532781895775558, 1.0194138950524914] | pass |
| full | aa | `compare_low_1000d_x100000` | 0.999 [0.955887722434682, 1.004745914551176] | pass |
| full | aa | `compare_equal_1000d_x100000` | 1.006 [0.9304922744811529, 1.0353522263323465] | pass |
| full | aa | `fibonacci_1000_x100` | 0.998 [0.9751344718038668, 1.0076357192742547] | pass |
| full | aa | `multiply_100d_x100000` | 1.002 [0.9942184845828163, 1.0392205744415028] | pass |
| full | aa | `multiply_10000d_x1000` | 0.998 [0.9958184855249032, 1.0000802506729147] | pass |
| full | aa | `sqrt_scale100_x5000` | 1.001 [0.9899242247420191, 1.0139021602940594] | pass |
| full | aa | `multiply_dense_10000d_x500` | 0.998 [0.9932271274235268, 1.0021995769681316] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.000 [0.9909786269869787, 1.0179677630765331] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.000 [0.9976680272717591, 1.0020204042638736] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 1.003 [0.9735248491220642, 1.0273108907519106] | pass |
| full | aa | `power_100d_e17_x1000` | 1.001 [0.9926804683263739, 1.0070714976470716] | pass |
| full | aa | `sparse_array_200000` | 0.983 [0.9678075154790475, 1.0112598358531577] | pass |
| full | aa | `decimal_input_4000d_x100` | 0.998 [0.9928118660798577, 1.0018016474394271] | pass |
| full | aa | `streaming_statements_20000` | 1.022 [0.9372782443579792, 1.1574451915530257] | inconclusive-noisy |
| full | paired | `startup` | 1.056 [0.99746200515034, 1.1346416745078143] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.586 [0.5742484368772919, 0.594902176347271] | pass |
| full | paired | `calls_200000` | 0.755 [0.7236297347129603, 0.7687871001381447] | pass |
| full | paired | `array_200000` | 0.639 [0.6217855012759288, 0.6619521631030447] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.555 [0.5379154622322777, 0.5562829243235136] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.771 [0.7485555018294129, 0.8737492030415959] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.784 [0.7663390071449944, 0.8095671985397097] | pass |
| full | paired | `fibonacci_1000_x100` | 0.776 [0.7446234540515515, 0.7870447259762662] | pass |
| full | paired | `multiply_100d_x100000` | 0.831 [0.8067788530505988, 0.8327322562594184] | pass |
| full | paired | `multiply_10000d_x1000` | 1.047 [1.0442983093110974, 1.0497546486785718] | pass |
| full | paired | `sqrt_scale100_x5000` | 1.064 [0.9815857907355228, 1.068119029300718] | inconclusive-noisy |
| full | paired | `multiply_dense_10000d_x500` | 1.021 [1.0178828910782256, 1.0280135377963224] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.935 [0.9049903313884629, 0.9403700816432491] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 1.080 [1.0667019879293433, 1.0881308909803393] | confirmed regression |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.990 [0.97198357865361, 1.0043823509383227] | pass |
| full | paired | `power_100d_e17_x1000` | 0.971 [0.9523382531507245, 0.9786192328194987] | pass |
| full | paired | `sparse_array_200000` | 0.572 [0.5520148722888846, 0.5934126846439177] | pass |
| full | paired | `decimal_input_4000d_x100` | 1.000 [0.9851723349491227, 1.0022268835435133] | pass |
| full | paired | `streaming_statements_20000` | 0.965 [0.7945926870410271, 0.9742610219468426] | pass |
