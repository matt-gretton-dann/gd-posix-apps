"""Reproduce canonical declaration checks on a Windows-style text checkout."""
from pathlib import Path
import subprocess,sys,tempfile,unittest
ROOT=Path(__file__).resolve().parents[2]
class DeclarationText(unittest.TestCase):
 def test_crlf_checkout_preserves_the_canonical_declaration(self):
  with tempfile.TemporaryDirectory() as tmp:
   path=Path(tmp)/'declaration.json'
   generated=subprocess.run([sys.executable,str(ROOT/'src/bc/scripts/generate-reconstruction-declaration.py'),'--output',str(path)],capture_output=True)
   self.assertEqual(generated.returncode,0,generated.stderr)
   raw=path.read_bytes().replace(b'\r\n',b'\n')
   path.write_bytes(raw.replace(b'\n',b'\r\n'))
   p=subprocess.run([sys.executable,str(ROOT/'src/bc/scripts/generate-reconstruction-declaration.py'),'--check','--output',str(path)],capture_output=True)
   self.assertEqual(p.returncode,0,p.stderr)
   path.write_bytes(path.read_bytes().replace(b'3beebb1461ac7549832707f9b84e547127e3c14a',b'0'*40))
   p=subprocess.run([sys.executable,str(ROOT/'src/bc/scripts/generate-reconstruction-declaration.py'),'--check','--output',str(path)],capture_output=True)
   self.assertNotEqual(p.returncode,0,'normalization must still reject changed declaration content')
if __name__=='__main__':unittest.main()
