# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.915 [0.8377786020144643, 1.1300288817452564] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.006 [0.9943032233479887, 1.015073308004812] | pass |
| full | aa | `calls_200000` | 0.999 [0.9989278920592526, 1.0199917490244188] | pass |
| full | aa | `array_200000` | 0.990 [0.9512991209928146, 1.0015181753828095] | pass |
| full | aa | `compare_high_1000d_x100000` | 1.002 [0.9895492946296552, 1.0221269215701712] | pass |
| full | aa | `compare_low_1000d_x100000` | 1.009 [1.00048883233807, 1.0201810546927388] | pass |
| full | aa | `compare_equal_1000d_x100000` | 1.006 [0.9911057258684657, 1.021810491761983] | pass |
| full | aa | `fibonacci_1000_x100` | 0.993 [0.9827321668433041, 1.0122231713748997] | pass |
| full | aa | `multiply_100d_x100000` | 1.012 [0.9952804560541244, 1.0261336687661526] | pass |
| full | aa | `multiply_10000d_x1000` | 1.006 [0.9967619244509247, 1.0122023384935341] | pass |
| full | aa | `sqrt_scale100_x5000` | 0.997 [0.9902972067511244, 1.0047689347869462] | pass |
| full | aa | `multiply_dense_10000d_x500` | 1.000 [0.9960979273787336, 1.0066275297763931] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.002 [0.9600452227753277, 1.0209567400916275] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.002 [0.9938445285640289, 1.0042988426148405] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 0.998 [0.9681580295621515, 1.027259026281886] | pass |
| full | aa | `power_100d_e17_x1000` | 1.003 [0.9904353570042088, 1.005655935824589] | pass |
| full | aa | `sparse_array_200000` | 1.013 [0.9864984178789636, 1.0317732295362276] | pass |
| full | aa | `decimal_input_4000d_x100` | 1.001 [0.9726047264679922, 1.0127030759874118] | pass |
| full | aa | `streaming_statements_20000` | 0.983 [0.9205750608040755, 1.0192115196508917] | pass |
| full | paired | `startup` | 1.111 [0.887159234688038, 1.196948063537234] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.985 [0.9797138204295014, 0.995053221033621] | pass |
| full | paired | `calls_200000` | 0.996 [0.9955529904704862, 1.032656706995208] | pass |
| full | paired | `array_200000` | 0.982 [0.9680745392700291, 0.9844215306549816] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.985 [0.9787367643812023, 0.9916164427990249] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.980 [0.9759123166399993, 1.0094169342767796] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.951 [0.9316505385540893, 0.9793900998651457] | pass |
| full | paired | `fibonacci_1000_x100` | 0.996 [0.9610907756113953, 0.9989629491986516] | pass |
| full | paired | `multiply_100d_x100000` | 0.979 [0.9516816041721213, 1.0049311718388343] | pass |
| full | paired | `multiply_10000d_x1000` | 0.885 [0.8815966309348267, 0.8883335845043595] | pass |
| full | paired | `sqrt_scale100_x5000` | 0.988 [0.9848978915565688, 1.007709101805402] | pass |
| full | paired | `multiply_dense_10000d_x500` | 0.892 [0.8696287472280121, 0.8975152616540021] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 1.008 [1.0049245307649912, 1.0177958172319088] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 0.345 [0.34210179862112927, 0.35023896928064885] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.960 [0.8661857026835471, 1.0242923537081892] | pass |
| full | paired | `power_100d_e17_x1000` | 0.996 [0.9864746005444982, 1.0101806538111031] | pass |
| full | paired | `sparse_array_200000` | 0.985 [0.9663915947341242, 1.0209330859945194] | pass |
| full | paired | `decimal_input_4000d_x100` | 1.000 [0.995142321499476, 1.0121898743430007] | pass |
| full | paired | `streaming_statements_20000` | 1.018 [0.9356115527351719, 1.0951584850549783] | inconclusive-noisy |
