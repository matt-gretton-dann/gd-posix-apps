# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.943 [0.908381016486202, 1.0982325631274417] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.008 [0.9949249088490263, 1.0133126404044057] | pass |
| full | aa | `calls_200000` | 0.982 [0.9687177421303074, 1.0220267782654462] | pass |
| full | aa | `array_200000` | 1.003 [0.9841099374442893, 1.037095120641702] | pass |
| full | aa | `compare_high_1000d_x100000` | 0.993 [0.9705282247191158, 1.02625251109646] | pass |
| full | aa | `compare_low_1000d_x100000` | 1.000 [0.965652076792658, 1.043071104888753] | pass |
| full | aa | `compare_equal_1000d_x100000` | 1.030 [0.9822150052542098, 1.050533439182576] | inconclusive-noisy |
| full | aa | `fibonacci_1000_x100` | 1.011 [0.9152556063878927, 1.099247708838378] | inconclusive-noisy |
| full | aa | `multiply_100d_x100000` | 0.998 [0.9842843718117251, 1.0316167914765575] | pass |
| full | aa | `multiply_10000d_x1000` | 1.000 [0.9961496039237947, 1.00209647822035] | pass |
| full | aa | `sqrt_scale100_x5000` | 0.989 [0.9675782347598242, 1.0051623347918355] | pass |
| full | aa | `multiply_dense_10000d_x500` | 1.001 [0.9954967700710725, 1.0021547063955871] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.001 [0.9913983601482083, 1.0019339097479008] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.002 [0.998567885664722, 1.0059508764224412] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 0.976 [0.9533683128902861, 1.040465521404453] | pass |
| full | aa | `power_100d_e17_x1000` | 1.002 [0.993420038333462, 1.0143637295600783] | pass |
| full | aa | `sparse_array_200000` | 0.984 [0.9556620949584879, 1.0546295935610104] | inconclusive-noisy |
| full | aa | `decimal_input_4000d_x100` | 1.003 [0.9968034753334747, 1.0166314903785947] | pass |
| full | aa | `streaming_statements_20000` | 0.987 [0.9686157145293383, 1.007605481298913] | pass |
| full | paired | `startup` | 1.054 [0.9164406072351956, 1.1366063348385231] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.990 [0.9788607239035041, 1.0122814381522391] | pass |
| full | paired | `calls_200000` | 1.000 [0.9692326231253929, 1.0333573670619234] | pass |
| full | paired | `array_200000` | 1.014 [0.9910714559425744, 1.0354201531887628] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.989 [0.952482541307302, 1.004592707084823] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.993 [0.9276298223285724, 1.013121226552301] | pass |
| full | paired | `compare_equal_1000d_x100000` | 1.032 [1.004367278349846, 1.1925734131771004] | inconclusive-noisy |
| full | paired | `fibonacci_1000_x100` | 0.989 [0.9702490512881133, 1.051185267375435] | inconclusive-noisy |
| full | paired | `multiply_100d_x100000` | 0.981 [0.9457330890246467, 1.0633104882853814] | inconclusive-noisy |
| full | paired | `multiply_10000d_x1000` | 0.909 [0.905004749170275, 0.9107371782537699] | pass |
| full | paired | `sqrt_scale100_x5000` | 1.175 [1.0268853128327715, 1.2272081680769529] | inconclusive-noisy |
| full | paired | `multiply_dense_10000d_x500` | 0.865 [0.8608365243558787, 0.868398000721518] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 1.002 [0.9988741801918808, 1.0329007297778132] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 0.351 [0.34971138750040676, 0.3530020089812541] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 1.261 [1.191906975008015, 1.3431329107795147] | confirmed regression |
| full | paired | `power_100d_e17_x1000` | 0.992 [0.9857986401761621, 1.0632447555071034] | inconclusive-noisy |
| full | paired | `sparse_array_200000` | 1.000 [0.9767111418191601, 1.0744878917489449] | inconclusive-noisy |
| full | paired | `decimal_input_4000d_x100` | 0.997 [0.9928317676956573, 1.0053289237678045] | pass |
| full | paired | `streaming_statements_20000` | 0.937 [0.8779173259494768, 1.0236454590218602] | pass |
