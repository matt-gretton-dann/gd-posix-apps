# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 1.029 [0.9490896687391854, 1.1540456270850934] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.002 [0.956300266001846, 1.011398610980527] | pass |
| full | aa | `calls_200000` | 1.004 [0.971247809578289, 1.1067686793347644] | inconclusive-noisy |
| full | aa | `array_200000` | 0.968 [0.930848400225617, 0.9989289837305199] | pass |
| full | aa | `compare_high_1000d_x100000` | 1.011 [1.000217996864695, 1.0305843899281004] | pass |
| full | aa | `compare_low_1000d_x100000` | 1.004 [0.9875628399171464, 1.0188907621444887] | pass |
| full | aa | `compare_equal_1000d_x100000` | 1.000 [0.9078607547724239, 1.009477217869415] | pass |
| full | aa | `fibonacci_1000_x100` | 0.995 [0.9816080246038967, 1.0178309591905144] | pass |
| full | aa | `multiply_100d_x100000` | 1.008 [0.9882005536930896, 1.0408653090282594] | pass |
| full | aa | `multiply_10000d_x1000` | 1.004 [0.9799212176749267, 1.0255338436402235] | pass |
| full | aa | `sqrt_scale100_x5000` | 0.985 [0.9461418817388542, 1.0098144417712016] | pass |
| full | aa | `multiply_dense_10000d_x500` | 1.007 [0.9592107365251076, 1.0433689973924314] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 0.991 [0.9230714310190882, 1.0069237262948747] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.017 [0.9758263251897722, 1.0605657554010575] | inconclusive-noisy |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 1.016 [0.9145604888034687, 1.0337973719827003] | pass |
| full | aa | `power_100d_e17_x1000` | 0.992 [0.9503021830568422, 1.0307988462038016] | pass |
| full | aa | `sparse_array_200000` | 1.000 [0.9516447772883694, 1.0028575948322698] | pass |
| full | aa | `decimal_input_4000d_x100` | 1.005 [0.9953789194543313, 1.0116719550271847] | pass |
| full | aa | `streaming_statements_20000` | 1.008 [0.9831812024046158, 1.0319835926539096] | pass |
| full | paired | `startup` | 1.147 [0.8909285569813489, 1.1606635237861147] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.735 [0.7131318010544301, 0.7450822466609848] | pass |
| full | paired | `calls_200000` | 0.790 [0.7832751542359557, 0.7936587602412541] | pass |
| full | paired | `array_200000` | 0.618 [0.6072289709758971, 0.636851281033285] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.674 [0.6671758141638677, 0.6820478450421605] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.827 [0.8151395945144552, 0.8340600853146215] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.825 [0.8166854117389046, 0.8322499325594569] | pass |
| full | paired | `fibonacci_1000_x100` | 0.820 [0.8158632010606808, 0.8314615560979303] | pass |
| full | paired | `multiply_100d_x100000` | 0.886 [0.8760359701509347, 0.9028030234456419] | pass |
| full | paired | `multiply_10000d_x1000` | 1.018 [0.9848158517023601, 1.038720119424872] | pass |
| full | paired | `sqrt_scale100_x5000` | 1.105 [1.0883096572156667, 1.1076162312608606] | confirmed regression |
| full | paired | `multiply_dense_10000d_x500` | 0.966 [0.9572498050464001, 1.0131405941398786] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.956 [0.9494638418155994, 0.9592543919806567] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 1.025 [0.9815073331723878, 1.0562494234932853] | inconclusive-noisy |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.946 [0.8807546236615508, 1.0104671652562593] | pass |
| full | paired | `power_100d_e17_x1000` | 0.971 [0.9611490607275566, 0.9741121030136521] | pass |
| full | paired | `sparse_array_200000` | 0.652 [0.6447389279913757, 0.6675777552751508] | pass |
| full | paired | `decimal_input_4000d_x100` | 0.993 [0.9560285812166588, 1.0061552758828542] | pass |
| full | paired | `streaming_statements_20000` | 0.996 [0.9850673651619754, 1.045576166571529] | pass |
