import json
import random
import statistics
import subprocess
import time

base = '/private/tmp/bc-200-sparse-codegen-build/src/bc/bc'
candidate = '/private/tmp/bc-207-binding-windows-build/src/bc/bc'
rng = random.Random(217579)
cases = []
for count in (1, 2, 3, 4, 6, 8, 12, 16, 20, 26):
    locals_ = 'abcdefghijklmnopqrstuvwyz'[:count-1]
    declaration = 'auto ' + ','.join(locals_) + '\n' if locals_ else ''
    body = 'define f(x) {\n' + declaration + 'return(x+1)\n}\nfor(i=0;i<200000;i++) y=f(i)\ny'
    cases.append(('scalar count '+str(count), body, 17))
for name, body, pairs in cases:
    payload = ('scale=20\n' + body + '\n').encode()
    rows = []
    for pair in range(pairs + 2):
        roles = [('base', base), ('aa', base), ('candidate', candidate)]
        rng.shuffle(roles)
        times, outputs = {}, set()
        for role, binary in roles:
            start = time.perf_counter_ns()
            result = subprocess.run([binary], input=payload, capture_output=True, check=True)
            times[role] = time.perf_counter_ns() - start
            assert not result.stderr, result.stderr
            outputs.add(result.stdout)
        assert len(outputs) == 1, outputs
        if pair >= 2:
            rows.append(times)
    with open('/private/tmp/bc-207-bindings-count-samples.jsonl', 'a') as raw:
        raw.write(json.dumps(dict(name=name, program=payload.decode(), rows=rows))+'\n')
    for role in ('aa', 'candidate'):
        ratios = [row[role] / row['base'] for row in rows]
        samples = sorted(statistics.median(rng.choices(ratios, k=pairs)) for _ in range(5000))
        print(name, role, round(statistics.median(ratios), 4), [round(samples[i], 4) for i in (125, 4874)],
              'base ms', round(statistics.median(row['base'] for row in rows)/1e6, 3), flush=True)
