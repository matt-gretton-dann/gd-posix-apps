import hashlib,json,os,platform,random,re,subprocess
from pathlib import Path
root=Path('/private/tmp/gd-posix-bc-binding-windows/docs/utilities/bc-207-binding-windows/local/arithmetic/workloads')
out=Path('/private/tmp/bc-207-binding-rss-checked');out.mkdir(exist_ok=True)
roles={'base':'/private/tmp/bc-200-sparse-codegen-build/src/bc/bc','candidate':'/private/tmp/bc-207-binding-windows-build/src/bc/bc'}
report={'platform':platform.platform(),'purpose':'native RSS diagnostics, not paired timing acceptance','roles':{k:{'path':v,'sha256':hashlib.sha256(Path(v).read_bytes()).hexdigest(),'bytes':Path(v).stat().st_size} for k,v in roles.items()},'rows':[]}
rng=random.Random(207590)
for name in ('loop_1000000','calls_200000','array_200000','fibonacci_1000_x100','recursion_50_x1000'):
 program=(root/(name+'.bc')).read_bytes();expected=(root/(name+'.expected')).read_text().split()
 for sample in range(7):
  order=list(roles);rng.shuffle(order)
  for role in order:
   r=subprocess.run(['/usr/bin/time','-l',roles[role]],input=program,capture_output=True,timeout=30,env={**os.environ,'LC_ALL':'C','BC_ENV_ARGS':''})
   assert r.returncode==0 and r.stdout.decode().replace(chr(92)+chr(10),'').split()==expected,(name,role,r.returncode)
   log=r.stderr.decode();match=re.search(r'(\d+)\s+maximum resident set size',log);assert match,log
   logfile=f'{name}-{sample}-{role}.time.txt';(out/logfile).write_bytes(r.stderr)
   report['rows'].append({'workload':name,'role':role,'sample':sample,'rss_bytes':int(match.group(1)),'time_log':logfile,'program_sha256':hashlib.sha256(program).hexdigest(),'stdout_sha256':hashlib.sha256(r.stdout).hexdigest()})
 (out/(name+'.bc')).write_bytes(program)
(out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
