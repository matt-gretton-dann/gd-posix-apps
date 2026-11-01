# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.875 [0.7910217840956041, 1.3029258541451458] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.008 [0.9519985774024821, 1.030312537118583] | pass |
| full | aa | `calls_200000` | 1.001 [0.9818929004044243, 1.0042211238643375] | pass |
| full | aa | `array_200000` | 1.000 [0.9856148644232509, 1.0104488208891111] | pass |
| full | aa | `compare_high_1000d_x100000` | 1.011 [0.9943416596041282, 1.0264686256859152] | pass |
| full | aa | `compare_low_1000d_x100000` | 1.001 [0.9693601767221047, 1.0345640523273776] | pass |
| full | aa | `compare_equal_1000d_x100000` | 1.025 [1.0010825428629473, 1.03465423493402] | pass |
| full | aa | `fibonacci_1000_x100` | 1.012 [0.9933917100160191, 1.041640345575331] | pass |
| full | aa | `multiply_100d_x100000` | 1.003 [0.9891928085296062, 1.0218816752160116] | pass |
| full | aa | `multiply_10000d_x1000` | 1.001 [0.9987410474072217, 1.0023806716200672] | pass |
| full | aa | `sqrt_scale100_x5000` | 0.998 [0.9896008468101916, 1.0060218629888364] | pass |
| full | aa | `multiply_dense_10000d_x500` | 1.001 [0.9982376354985808, 1.0028186157924759] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.000 [0.9988391277058413, 1.0011237555554455] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.000 [0.9982252276244226, 1.000126201945231] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 0.989 [0.9819758213517006, 1.0218417782268299] | pass |
| full | aa | `power_100d_e17_x1000` | 0.999 [0.9926907628599272, 1.0072536554972018] | pass |
| full | aa | `sparse_array_200000` | 1.013 [0.9795477357830743, 1.038402094865844] | pass |
| full | aa | `decimal_input_4000d_x100` | 1.000 [0.9959476470898867, 1.0015684590970633] | pass |
| full | aa | `streaming_statements_20000` | 0.997 [0.9895742466010079, 1.0031089385653245] | pass |
| full | paired | `startup` | 1.190 [0.8375750671864559, 1.3218413603896502] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.816 [0.7842523420953965, 0.833555739189896] | pass |
| full | paired | `calls_200000` | 0.893 [0.8666005145912757, 0.9131088368751884] | pass |
| full | paired | `array_200000` | 0.823 [0.8034293979594235, 0.8298188994655376] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.829 [0.8039120834585999, 0.8384935232892248] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.987 [0.9530638618462076, 0.9956584333181251] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.982 [0.9653786677651642, 0.9979282243382525] | pass |
| full | paired | `fibonacci_1000_x100` | 0.886 [0.8741844391547272, 0.8972801131050001] | pass |
| full | paired | `multiply_100d_x100000` | 0.941 [0.9229495025544973, 0.9487308201884679] | pass |
| full | paired | `multiply_10000d_x1000` | 1.015 [1.0133863788782744, 1.016360645250215] | pass |
| full | paired | `sqrt_scale100_x5000` | 1.076 [1.070691132261648, 1.0770843149002236] | confirmed regression |
| full | paired | `multiply_dense_10000d_x500` | 1.004 [1.001813115464468, 1.0048539882516025] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.968 [0.9656693536036192, 0.9692170342643772] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 1.078 [1.0747939879258126, 1.0809902060608008] | confirmed regression |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 1.002 [0.9778693134192755, 1.0322273594349454] | pass |
| full | paired | `power_100d_e17_x1000` | 0.976 [0.9716320975381946, 0.9870144604002437] | pass |
| full | paired | `sparse_array_200000` | 0.834 [0.8140395165280229, 0.8551202752943179] | pass |
| full | paired | `decimal_input_4000d_x100` | 1.001 [0.9982346230564731, 1.0026089556171276] | pass |
| full | paired | `streaming_statements_20000` | 1.001 [0.9960704504117942, 1.0229732912161773] | pass |
