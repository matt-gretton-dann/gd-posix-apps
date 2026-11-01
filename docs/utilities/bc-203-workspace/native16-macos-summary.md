# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.991 [0.8439095140151989, 1.374744728807556] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.027 [0.991070299987503, 1.0924752372535673] | inconclusive-noisy |
| full | aa | `calls_200000` | 0.950 [0.9002041982603711, 1.0572051072998319] | inconclusive-noisy |
| full | aa | `array_200000` | 0.927 [0.8698887393106145, 1.0325396972670884] | pass |
| full | aa | `compare_high_1000d_x100000` | 0.991 [0.8035307676196127, 1.0546966309241332] | inconclusive-noisy |
| full | aa | `compare_low_1000d_x100000` | 1.015 [0.76212058939294, 1.0755696575878648] | inconclusive-noisy |
| full | aa | `compare_equal_1000d_x100000` | 0.921 [0.868599638484805, 1.0641925879328076] | inconclusive-noisy |
| full | aa | `fibonacci_1000_x100` | 0.966 [0.861088088642039, 1.1989309831432085] | inconclusive-noisy |
| full | aa | `multiply_100d_x100000` | 0.991 [0.831594103789743, 1.0395438155321093] | pass |
| full | aa | `multiply_10000d_x1000` | 0.971 [0.9379651670918201, 1.048412018854181] | pass |
| full | aa | `sqrt_scale100_x5000` | 1.071 [0.994965787774744, 1.1278631845608431] | inconclusive-noisy |
| full | aa | `multiply_dense_10000d_x500` | 1.002 [0.9453357394321573, 1.020534557510974] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 0.988 [0.9067247186042806, 1.0443452647453386] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.005 [0.9095625150175354, 1.0137544503193956] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 1.151 [0.9421282715465767, 1.3289775490669253] | inconclusive-noisy |
| full | aa | `power_100d_e17_x1000` | 0.954 [0.9095800853885703, 1.0345848950594487] | pass |
| full | aa | `sparse_array_200000` | 0.945 [0.9167872705392633, 1.0782784520760804] | inconclusive-noisy |
| full | aa | `decimal_input_4000d_x100` | 1.014 [0.9807935194095043, 1.0841282319194145] | inconclusive-noisy |
| full | aa | `streaming_statements_20000` | 0.905 [0.8049043274015784, 1.0544668822936536] | inconclusive-noisy |
| full | paired | `startup` | 1.113 [0.9684156258824023, 1.179216862656501] | inconclusive-noisy |
| full | paired | `loop_1000000` | 1.000 [0.97450208408323, 1.008542666211499] | pass |
| full | paired | `calls_200000` | 0.993 [0.904335247223155, 1.0026861506888038] | pass |
| full | paired | `array_200000` | 1.033 [0.9500861268761176, 1.1220864176471779] | inconclusive-noisy |
| full | paired | `compare_high_1000d_x100000` | 0.998 [0.808971339387345, 1.0276951433702142] | pass |
| full | paired | `compare_low_1000d_x100000` | 1.007 [0.9738207329257547, 1.1097805813854222] | inconclusive-noisy |
| full | paired | `compare_equal_1000d_x100000` | 0.994 [0.9165555628584344, 1.0648465037940258] | inconclusive-noisy |
| full | paired | `fibonacci_1000_x100` | 0.984 [0.891479224007245, 1.0077665465701502] | pass |
| full | paired | `multiply_100d_x100000` | 1.001 [0.9230082298485045, 1.0038963407042591] | pass |
| full | paired | `multiply_10000d_x1000` | 0.878 [0.8486597672423538, 0.883097562623439] | pass |
| full | paired | `sqrt_scale100_x5000` | 1.003 [0.954282291494508, 1.0088658922406704] | pass |
| full | paired | `multiply_dense_10000d_x500` | 0.870 [0.850889010693457, 0.8978457208441296] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 1.004 [0.9959409403018754, 1.0624530887233752] | inconclusive-noisy |
| full | paired | `multiply_sparse_10000d_x1000` | 0.340 [0.3191993298942453, 0.3468259235755596] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 0.973 [0.8853742812436913, 1.012499149536882] | pass |
| full | paired | `power_100d_e17_x1000` | 0.971 [0.9043931198650244, 1.0122201709323224] | pass |
| full | paired | `sparse_array_200000` | 1.006 [0.9724938861070489, 1.0208889649982509] | pass |
| full | paired | `decimal_input_4000d_x100` | 0.999 [0.9414677488926047, 1.080435275335542] | inconclusive-noisy |
| full | paired | `streaming_statements_20000` | 0.979 [0.9477892964024961, 0.9956989035545817] | pass |
