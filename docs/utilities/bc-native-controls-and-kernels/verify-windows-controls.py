"""Audit fixed Windows deployment evidence; no elapsed work or causal inference."""
import collections
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import sys
sys.dont_write_bytecode = True
import zipfile

ROOT=Path(__file__).resolve().parent
head='1089d716d229375b4d8dd435050c7c71b8d536e5'
base='d1de5150e511fcc75d718a3fb8d5f680219bafff'
import importlib.machinery
loader=importlib.machinery.SourceFileLoader('frozen_image_policy',str(ROOT/'sources/benchmark-policy.py.txt'))
spec=importlib.util.spec_from_loader(loader.name,loader)
policy=importlib.util.module_from_spec(spec);spec.loader.exec_module(policy)
def sha(data):return hashlib.sha256(data).hexdigest()
def checkout(name):
 return (ROOT/'sources'/(name+'.txt')).read_bytes().replace(b'\n',b'\r\n')
results=[]
for p in sorted((ROOT/'further-local/windows-controls').glob('replica*.zip')):
 with zipfile.ZipFile(p) as z:
  def read(name):return json.loads(z.read(name))
  d=read('decisions.json');host=read('host.json')
  assert d['status']=='diagnostic complete; source adoption and causal attribution unproven'
  assert d['base']==base and d['performance_acceptance'] is False
  assert (d['samples'],d['warmups'],d['deployments'],d['probe_launches_per_deployment'])==(9,3,6,3)
  assert d['controller_sha256']==sha(checkout('benchmark-image-controls.py'))
  assert d['probe_sha256']==sha(checkout('windows-image-probe.py'))
  assert d['policy_sha256']==sha(checkout('benchmark-policy.py'))
  images=d['images'];assert len(images)==6
  assert len({r['file_identity_sha256'] for r in images})==6
  image_hashes={r['sha256'] for r in images};assert len(image_hashes)==1
  source=read('source-implementations.json')['implementations'][0]['metadata']
  assert source['source_revision']==base and source['instrumented'] is False
  assert source['binary_sha256'] in image_hashes
  assert source['configuration']['build_type']=='Release'
  assert source['configuration']['multiply_split_point']==126
  assert source['flags']['compile_commands_sha256']==sha(z.read('source-compile_commands.json'))
  assert source['configuration']['threshold_header_sha256']==sha(z.read('source-multiply-threshold.hh'))
  assert read('source-build-manifest.json')['status']=='built; run checked corpus before timing'
  for image in images:assert sha(z.read('images/'+image['path']))==image['sha256']
  probes=d['probes'];assert len(probes)==18
  assert {r['trial'] for r in probes}=={0,1,2}
  for image in range(6):
   observations=[r for r in probes if r['image']=='image'+str(image)]
   assert len(observations)==3 and {r['trial'] for r in observations}=={0,1,2}
   for r in observations:
    assert r['status']=='collected' and r['checked'] is True
    assert r['main_module_path_matches'] and r['image_bytes']>0 and int(r['module_base'],16)>0
    assert r['binary_sha256'] in image_hashes
    assert r['program_sha256']==sha(b'1+1\n')
    assert r['output_sha256'] in (sha(b'2\n'),sha(b'2\r\n'))
  checked=0
  for preset in ('full','holdout'):
   prefix=preset+'-paired/'
   manifest=read(prefix+'manifest.json');prepared=read('prepared/'+preset+'/manifest.json')
   assert manifest['corpus']==prepared['corpus'] and len(manifest['corpus'])==7
   assert manifest['reference']=='image0' and (manifest['samples'],manifest['warmups'])==(9,3)
   assert manifest['harness_sha256']==sha(checkout('bench-bc.py'))
   corpus={r['name']:r for r in manifest['corpus']}
   for name,case in corpus.items():
    for field,ext in (('program_sha256','bc'),('oracle_sha256','expected')):
     content=z.read(prefix+'workloads/'+name+'.'+ext)
     assert sha(content)==case[field]
     assert content==z.read('prepared/'+preset+'/workloads/'+name+'.'+ext)
   raw=[json.loads(line) for line in z.read(prefix+'samples.jsonl').splitlines()]
   assert len(raw)==504
   counts=collections.Counter()
   for r in raw:
    assert r['checked'] is True and math.isfinite(r['seconds']) and r['seconds']>=0
    oracle=z.read(prefix+'workloads/'+r['workload']+'.expected')
    wrapped=b''
    for line in oracle.splitlines():
     width=68 if len(line)>69 else 69
     while len(line)>=width:
      wrapped+=line[:width]+b'\\\n';line=line[width:]
     wrapped+=line+b'\n'
    assert r['output_sha256'] in {sha(content) for content in (oracle,wrapped) for content in (content,content.replace(b'\n',b'\r\n'))}
    assert r['implementation'] in {'image'+str(n) for n in range(6)}
    counts[r['workload'],r['implementation'],r['phase'],r['round']]+=1
   assert len(counts)==504 and set(counts.values())=={1}
   for name in corpus:
    for image in range(6):
     for phase,number in (('warmup',3),('measurement',9)):
      assert {r for (n,i,p,r) in counts if (n,i,p)==(name,'image'+str(image),phase)}==set(range(number))
   checked+=len(raw)
   rows=read(prefix+'summary.json')['workloads']
   assert {r['name'] for r in rows}==set(corpus)
   assert len([r for r in d['results'] if r['preset']==preset])==35
   for r in d['results']:
    if r['preset']!=preset:continue
    row=next(v for v in rows if v['name']==r['name'])
    assert r['first']==row['candidate_over_reference'][r['image']]
    assert r['confirmation'] is None
    assert r['status']==('inconclusive' if r['name']=='startup' else policy.aa_status(r['first'],None))
    assert r['name']=='startup' or policy.aa_direction(r['first'])==0
  cpu=json.loads(host['details'][0]['stdout'])['Name']
  record=dict(replica=d['replica'],archive=p.name,archive_sha256=sha(p.read_bytes()),cpu=cpu,
              checked_executions=checked,module_probes=len(probes),images=6,
              statuses=dict(collections.Counter(r['status'] for r in d['results'])),
              module_summary=d['module_summary'],performance_acceptance=False,
              scope='exact bytes/provenance/raw-output checks and policy replay; reported confidence intervals are not independently bootstrap-recomputed')
  results.append(record)
  print('replica',d['replica'],cpu,checked,'checked;',record['statuses'])
assert len(results)==3 and {r['replica'] for r in results}=={'1','2','3'}
# This report is hashed evidence. Write explicit UTF-8 bytes so replay on
# Windows cannot translate its LF newlines and invalidate the recorded hash.
(ROOT/'windows-controls-audit.json').write_bytes((json.dumps(dict(run=41506823190,head=head,base=base,results=results),indent=2)+'\n').encode('utf-8'))
print('all three complete; no timing bias reproduced; no causal attribution or old-loss waiver')
