import hashlib,json,os,subprocess
from pathlib import Path
out=Path('/private/tmp/bc-207-binding-allocations');out.mkdir(exist_ok=False)
root=Path('/private/tmp/gd-posix-bc-binding-windows/docs/utilities/bc-207-binding-windows/local/arithmetic/workloads')
roles={'off':'/private/tmp/bc-207-binding-profile-off-build/src/bc/bc','on':'/private/tmp/bc-207-binding-profile-build/src/bc/bc'}
cases={name:((root/(name+'.bc')).read_bytes(),(root/(name+'.expected')).read_text().split()) for name in ('loop_1000000','calls_200000','recursion_50_x1000')}
cases['cold-one-call']=(b'define f(x) { return(x*x+1) }\nf(4)\n',['17'])
autos=','.join(c for c in 'abcdefghijklmnopqrstuvwxyz' if c!='x')
cases['wide-recursive-26']=(('define f(x) {\nauto '+autos+'\nif(x>0) return(f(x-1)+1)\nreturn(0)\n}\nf(50)\n').encode(),['50'])
report={'source_revision':'5019aea78164102541fa14b2d1716aed61c7177c','purpose':'instrumented selected-allocation diagnostics, not elapsed-time/RSS evidence','roles':{k:{'path':v,'bytes':Path(v).stat().st_size,'sha256':hashlib.sha256(Path(v).read_bytes()).hexdigest()} for k,v in roles.items()},'cases':[]}
for name,(program,expected) in cases.items():
 (out/(name+'.bc')).write_bytes(program)
 rows={}
 for role,binary in roles.items():
  path=out/(name+'-'+role+'.json')
  p=subprocess.run([binary],input=program,capture_output=True,timeout=60,env={**os.environ,'GD_BC_PROFILE_OUTPUT':str(path),'LC_ALL':'C','BC_ENV_ARGS':''})
  assert p.returncode==0 and not p.stderr and p.stdout.decode().replace(chr(92)+chr(10),'').split()==expected,(name,role,p.stderr)
  d=json.loads(path.read_text());assert d['total']['live_bytes']==0
  rows[role]={'total':d['total'],'frames':d['allocations']['frames'],'events':{k:d['events'][k] for k in ('digit_copies','digit_moves','function_calls','instructions_executed')}}
 report['cases'].append({'name':name,'program_sha256':hashlib.sha256(program).hexdigest(),'roles':rows})
(out/'summary.json').write_text(json.dumps(report,indent=2)+'\n')
