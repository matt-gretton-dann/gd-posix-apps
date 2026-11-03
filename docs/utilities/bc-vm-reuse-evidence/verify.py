#!/usr/bin/env python3
"""Verify retained VM mode costs without executing archived code or binaries."""
from collections import Counter
from fractions import Fraction
import hashlib
import json
from pathlib import Path
import re
import statistics
import zipfile

HERE = Path(__file__).resolve().parent


def digest(data):
    return hashlib.sha256(data).hexdigest()


def require(condition, message):
    if not condition:
        raise ValueError(message)


def load():
    summary = json.loads((HERE / 'rss-v1.json').read_text())
    archive = (HERE / 'rss-v1.zip').read_bytes()
    require(len(archive) == summary['archive_bytes'] and digest(archive) == summary['archive_sha256'],
            'archive identity differs')
    with zipfile.ZipFile(HERE / 'rss-v1.zip') as source:
        names = source.namelist()
        require(len(names) == len(set(names)), 'duplicate ZIP members')
        require(all(not name.startswith('/') and '..' not in name.split('/') for name in names),
                'unsafe ZIP member')
        payloads = {name: source.read(name) for name in names}
    return payloads, summary


def numbers(data):
    lines = data.decode('ascii').replace('\\\r\n', '').replace('\\\n', '').splitlines()
    require(all(re.fullmatch(r'-?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+)', line) for line in lines),
            'invalid numeric output')
    return [Fraction(line) for line in lines]


def oracle(case):
    program, parameters = case['program'], case['parameters']
    a_text = re.search(r'(?:^|\n)a=([0-9.]+)\n', program).group(1)
    b_text = re.search(r'\nb=([0-9.]+)\n', program).group(1)
    a, b = Fraction(a_text), Fraction(b_text)
    count, places = parameters['iterations'], parameters.get('scale', 0)
    require(type(count) is int and count > 0 and type(places) is int and places >= 0,
            'invalid fixture dimensions')
    if case['group'].startswith('divisor_'):
        shape, operation = parameters['admission'], parameters['operation']
        require(shape in ('changing', 'temporary', 'periodic', 'cold') and operation in ('divide', 'modulo'),
                'invalid admission class')
        require(case['group'] == 'divisor_' + shape + '_' + operation and places == 0,
                'admission class differs')
        require(count == 1 if shape == 'cold' else count >= 32, 'cold/periodic dimensions differ')
        prefix = 'b+=1;' if shape == 'changing' else 'j+=1;if(j==16) {b+=1;j=0};' if shape == 'periodic' else ''
        operand = '(b+i)' if shape == 'temporary' else 'b'
        symbol = '/' if operation == 'divide' else '%'
        expected_program = ('scale=0\na={}\nb={}\ns=0\nj=0\n'
                            'for(i=1;i<={};i++) {{{}c=a{}{};s+=c}}\nc\ns\n').format(
                                a_text, b_text, count, prefix, symbol, operand)
        require(program == expected_program, 'admission program differs from declared requests')
        values = []
        for i in range(1, count + 1):
            offset = i if shape in ('changing', 'temporary') else i // 16 if shape == 'periodic' else 0
            divisor = b + offset
            quotient = int(a / divisor)
            values.append(Fraction(quotient) if operation == 'divide' else a - quotient * divisor)
        return [values[-1], sum(values)]
    operation = '/' if case['group'] == 'divide_multilimb' else '%'
    prefix = 'scale={}\n'.format(places) if program.startswith('scale=') else ''
    require(prefix or places == 0, 'missing scale selection')
    require(program == prefix + 'a={}\nb={}\nfor(i=0;i<{};i++) c=a{}b\nc\n'.format(
        a_text, b_text, count, operation), 'stable program differs from declared requests')
    quotient = Fraction(int(a / b * 10 ** places), 10 ** places)
    return [quotient if operation == '/' else a - quotient * b]


def verify(payloads, summary=None):
    manifest = json.loads(payloads['manifest.json'])
    require(manifest['adoption_accepted'] is False and (summary is None or summary['adoption_accepted'] is False),
            'RSS observations do not establish adoption')
    require(re.fullmatch('[0-9a-f]{40}', manifest['source_revision']), 'invalid source identity')
    require(set(payloads) == set(manifest['files']) | {'manifest.json'}, 'file inventory differs')
    for name, expected in manifest['files'].items():
        require(digest(payloads[name]) == expected, 'payload hash differs: ' + name)
    roles = {role['name']: role for role in manifest['roles']}
    require(len(manifest['roles']) == 2 and set(roles) == {'off', 'on'}, 'mode inventory differs')
    binaries = []
    code = json.loads(payloads['codegen/manifest.json'])['observations']
    require(len(code) == 2 and {row['name'] for row in code} == set(roles), 'code inventory differs')
    for mode, role in roles.items():
        metadata = role['metadata']
        require(metadata == json.loads(payloads[mode + '/metadata.json']), 'role metadata differs')
        require(metadata['source_revision'] == manifest['source_revision'] and metadata['instrumented'] is False,
                'timing source or instrumentation differs')
        configuration = metadata['configuration']
        policy = configuration['prepared_divisors_policy']
        require(policy['status'] == 'recorded' and policy['mode'] == mode.upper() and
                policy['enabled'] is (mode == 'on'), 'mode activation differs')
        require(configuration['build_type'] == 'Release' and configuration['multiply_split_point'] == 126 and
                configuration['extensions'] == 'OFF', 'build configuration differs')
        commands = payloads[mode + '/compile_commands.json']
        require(digest(commands) == policy['verification']['sha256'] == metadata['flags']['compile_commands_sha256'],
                'compiled mode evidence differs')
        require(not re.search(rb'-fsanitize|-fprofile(?:-instr)?-generate|--coverage|GD_BC_PROFILE=1', commands),
                'instrumented command in RSS mode')
        anchors = set()
        for row in json.loads(commands):
            filename = row['file'].replace('\\', '/')
            if filename.endswith(('/src/bc/vm.cc', '/src/bc/bc.cc')):
                values = re.findall(r'(?:^|\s)-DGD_BC_PREPARED_DIVISORS=([^\s]+)', row['command'])
                require(values == ['1' if mode == 'on' else '0'], 'compiled activation differs')
                anchors.add(filename.rsplit('/', 1)[-1])
        require(anchors == {'vm.cc', 'bc.cc'}, 'compiled anchors missing')
        row = next(item for item in code if item['name'] == mode)
        binary = payloads[mode + '/bc']
        require(digest(binary) == metadata['binary_sha256'] == row['binary_sha256'] and
                binary == payloads['codegen/' + row['binary']] and len(binary) == row['file_bytes'],
                'binary identity differs')
        inspection = row['disassembly']
        require(inspection['status'] == 'collected' and len(inspection['commands']) == 2, 'inspection incomplete')
        for command in inspection['commands']:
            log = payloads['codegen/' + mode + '/' + command['log']]
            require(command['returncode'] == 0 and digest(log) == command['log_sha256'] and
                    len(log) == command['log_bytes'], 'inspection log differs')
        header = payloads['codegen/' + mode + '/' + inspection['commands'][0]['log']].decode()
        text_bytes = int(re.search(r'sectname __text\s+segname __TEXT\s+addr 0x[0-9a-f]+\s+size (0x[0-9a-f]+)', header).group(1), 16)
        binaries.append(dict(mode=mode, file_bytes=len(binary), text_section_bytes=text_bytes,
                             sha256=digest(binary)))
    fixtures = {case['name']: case for case in manifest['fixtures']}
    require(len(fixtures) == len(manifest['fixtures']) == 26, 'fixture inventory differs')
    groups = Counter(divide_multilimb=2, modulo=1, modulo_scaled_numerator=1, modulo_scaled_divisor=1)
    groups.update({'divisor_' + shape + '_' + op: 1 for shape in ('changing', 'temporary', 'periodic', 'cold')
                   for op in ('divide', 'modulo')})
    for preset in ('full', 'holdout'):
        require(Counter(case['group'] for case in fixtures.values() if case['preset'] == preset) == groups,
                'full/holdout class inventory differs')
    oracles = {name: oracle(case) for name, case in fixtures.items()}
    for name, case in fixtures.items():
        require(list(map(Fraction, case['expected'])) == oracles[name], 'fixture oracle differs')
    seen = set()
    observations = manifest['observations']
    for row in observations:
        key = (row['case'], row['mode'], row['trial'])
        require(key not in seen, 'duplicate observation')
        seen.add(key)
        require(row['case'] in fixtures and row['preset'] == fixtures[row['case']]['preset'] and
                row['checked'] is True and row['returncode'] == 0, 'unchecked or mislabelled observation')
        require(payloads[row['stderr']] == b'' and numbers(payloads[row['stdout']]) == oracles[row['case']],
                'observed output differs from independent oracle')
        resource = payloads[row['resource']].decode()
        rss = int(re.search(r'^\s*(\d+)\s+maximum resident set size\s*$', resource, re.M).group(1))
        require(rss == row['rss_bytes'] and rss > 0, 'RSS resource differs')
    require(seen == {(name, mode, trial) for name in fixtures for mode in roles for trial in range(3)},
            'incomplete observation grid')
    medians = []
    for name in sorted(fixtures):
        values = {mode: statistics.median(row['rss_bytes'] for row in observations
                                         if row['case'] == name and row['mode'] == mode) for mode in roles}
        medians.append(dict(name=name, median_peak_rss_bytes=values, on_minus_off_bytes=values['on'] - values['off']))
    result = dict(checked=len(observations), fixtures=len(fixtures), files=len(manifest['files']),
                  binaries=sorted(binaries, key=lambda row: row['mode']), medians=medians,
                  source_revision=manifest['source_revision'])
    require(summary is None or result == summary['results'], 'published observations differ')
    return result


if __name__ == '__main__':
    payloads, summary = load()
    result = verify(payloads, summary)
    print('{} checked processes; {} fixtures; adoption remains unestablished'.format(result['checked'], result['fixtures']))
