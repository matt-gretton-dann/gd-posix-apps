# Combining uniform runs and subtractive Karatsuba

The Apple Clang 21.0.0 arm64 Release comparison used ThinLTO and a fixed
split point of 126. The builds were the frontend baseline `72485e34`,
uniform runs `3899f02a`, subtractive Karatsuba `18260d62`, and a clean
merge of those two candidates without additional source edits. The
combined build passed 28 local arithmetic-oracle and VM CLI tests.

The benchmark used the arithmetic full and holdout presets restricted to
names beginning with `multiply_`, `square_`, `factorial_`, or `fibonacci_`.
It ran the four GD builds and pinned Howard 7.0.3, with nine measured
launches and one warmup per role and workload. There were 14 workloads
per preset and 1,400 checked outputs in total.

Across the two presets, the combined median ratios ranged from 0.982
to 1.010 against uniform runs alone. For the repeated 10,000-digit
product, uniform runs took about 4.2% of baseline time and adding
Karatsuba gave a ratio of 1.007 [0.981, 1.038] against uniform runs.
The held-out repeated product gave a combined ratio of 1.004.
The dense cases gave no clear additional gain. This one-host comparison
had no separate A/A control and leaves the combined kernel experimental.

The [full](full.jsonl.gz) and
[holdout](holdout.jsonl.gz) archives
retain the checked samples. The
[ratios](ratios.csv) report each build
against the uniform-run reference.
