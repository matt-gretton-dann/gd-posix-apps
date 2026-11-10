#!/usr/bin/env python3
"""Freeze fresh exact-integer validation without measuring implementations.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import random

SEED = int('9ac41aa0b2e23444dcaa12bdb8cfd9969add2568',16)^207197200


def cases(small_counts=False):
    rng=random.Random(SEED)
    calls=(1<<22)+rng.randrange(1<<12)
    repetitions=1<<18
    if small_counts:
        calls,repetitions=7,2  # Functional test only; no production CLI override.
    u,v=rng.randrange(1,10),rng.randrange(1,10)
    factorial=61+rng.randrange(1,8)
    start,stop,step=69+rng.randrange(2,10),3+rng.randrange(2),3
    addition=211+rng.randrange(25)
    terminal=start
    product=1
    while terminal>stop:
        product*=terminal
        terminal-=step
    product*=terminal
    summation=-4
    remaining=addition
    while remaining>2:
        summation+=remaining
        remaining-=3
    square_sum=calls*(calls-1)*(2*calls-1)//6
    linear_sum=calls*(calls-1)//2
    specs=[
        ('word-square','calls','x','return(x*x+1)',calls,square_sum+calls,True),
        ('word-shift','calls','x','return((x+{})*(x+{}))'.format(u,v),calls,
         square_sum+(u+v)*linear_sum+u*v*calls,True),
        ('factorial','recursion','n','if(n<=1)return(1)\nreturn(n*f(n-1))',
         factorial,math.factorial(factorial),False),
        ('stepped-product','recursion','n',
         'if(n<={})return(n)\nreturn(f(n-{})*n)'.format(stop,step),start,product,False),
        ('stepped-sum','recursion','n','if(n<=2)return(-4)\nreturn(n+f(n-3))',
         addition,summation,False)]
    rows=[]
    for name,group,parameter,body,input_value,expected,word in specs:
        program='scale=0\n{}=73\ndefine f({}) {{\n{}\n}}\n'.format(parameter,parameter,body)
        if word:
            program+='r=0\nfor(i=0;i<{};i++) r+=f(i)\n'.format(calls)
        else:
            program+='for(i=0;i<{};i++) r=f({})\n'.format(repetitions,input_value)
        program+='r\n{}\nscale\nibase\n'.format(parameter)
        rows.append(dict(name='integer_validation_'+name,group=group,program=program,
            expected=[str(expected),'73','0','10'],parameters=dict(mode='repeated',
                iterations=calls if word else repetitions,argument=input_value,
                scale=0,duration_target_seconds=.25,
                calibration='fixed a priori; no elapsed calibration or repair',
                exposure='fresh seeded validation before first elapsed observation')))
    return rows


def write(directory):
    directory.mkdir(parents=True,exist_ok=False)
    work=directory/'workloads'
    work.mkdir()
    records=[]
    for row in cases():
        program=row['program'].encode('utf-8')
        oracle=('\n'.join(row['expected'])+'\n').encode('utf-8')
        (work/(row['name']+'.bc')).write_bytes(program)
        (work/(row['name']+'.expected')).write_bytes(oracle)
        records.append(dict(name=row['name'],group=row['group'],parameters=row['parameters'],
            program_sha256=hashlib.sha256(program).hexdigest(),
            oracle_sha256=hashlib.sha256(oracle).hexdigest()))
    manifest=dict(kind='unmeasured-integer-inputs-v1',suite='arithmetic',preset='holdout',
        corpus_seed=SEED,corpus=records,generator_sha256=hashlib.sha256(
            Path(__file__).read_bytes().replace(b'\r\n',b'\n')).hexdigest(),
        scope='Five fixed exact-integer validation workloads; not prior performance evidence.')
    (directory/'manifest.json').write_bytes((json.dumps(manifest,indent=2,sort_keys=True)+'\n').encode())


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output',type=Path,required=True)
    write(parser.parse_args().output)
