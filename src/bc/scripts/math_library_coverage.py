"""Instrument this library's structured decisions in ordinary test-side bc.

The transformer accepts only the source shapes it understands. It evaluates
each original condition once. Local array flags distinguish loop exhaustion
from break, and return skips unreachable probes. No VM hooks are required.
This measures interpreted-library decisions, not C++ or compiler MC/DC.
"""
import re

CONTROL = re.compile(r'^( *)(if|while|for)\s*\(')
MARKER = '@GD-DECISION '


def instrument(source):
    lines = source.splitlines()
    decisions = []
    functions = [(index, match[1]) for index, line in enumerate(lines)
                 for match in [re.match(r'^define ([a-z])\(', line)] if match]

    def probe(number, outcome, indent):
        return [indent + '"{}{} {} "'.format(MARKER, number, outcome), indent + '0']

    def statement(line, loop):
        if line.strip() == 'break':
            if loop is None:
                raise ValueError('break outside a recognized loop')
            indent = line[:len(line) - len(line.lstrip())]
            return [indent + 'z[{}]=1'.format(loop), line]
        return [line]

    def block(start, stop, loop=None):
        result = []
        index = start
        while index < stop:
            line = lines[index]
            match = CONTROL.match(line)
            if not match:
                if re.match(r'^ *auto ', line):
                    if re.search(r'\bz\b', line):
                        raise ValueError('instrumentation local conflicts with source')
                    line = line.replace(';', ',z[];')
                if re.search(r'\belse\b', line) and not line.lstrip().startswith('*'):
                    raise ValueError('source else needs a supported transformation')
                result.extend(statement(line, loop))
                index += 1
                continue
            indent, kind = match[1], match[2]
            opening = line.index('(', match.start())
            depth, closing = 0, None
            for offset in range(opening, len(line)):
                depth += (line[offset] == '(') - (line[offset] == ')')
                if depth == 0:
                    closing = offset
                    break
            if closing is None:
                raise ValueError('multiline condition needs an explicit transformation')
            tail = line[closing + 1:].strip()
            number = index + 1
            function = next((name for beginning, name in reversed(functions)
                             if beginning < index), None)
            decisions.append(dict(line=number, function=function, kind=kind,
                                  expression=line[opening + 1:closing]))
            child_loop = number if kind != 'if' else loop
            if tail == '{':
                depth, end = 1, index + 1
                while end < stop and depth:
                    depth += lines[end].count('{') - lines[end].count('}')
                    if depth:
                        end += 1
                if depth or lines[end].strip() != '}':
                    raise ValueError('unrecognized structured block ending')
                body = block(index + 1, end, child_loop)
                following = end + 1
            elif kind == 'if' and tail:
                body = statement(indent + '  ' + tail, child_loop)
                following = index + 1
            else:
                raise ValueError('unrecognized control statement')
            result.append(indent + 'z[{}]=0'.format(number))
            result.append(line[:closing + 1] + ' {')
            if kind == 'if':
                result.append(indent + '  z[{}]=1'.format(number))
            result.extend(probe(number, 'true', indent + '  '))
            result.extend(body)
            if kind == 'if':
                result.append(indent + '}')
                result.append(indent + 'if (z[{}]==0) {{'.format(number))
                result.extend(probe(number, 'false', indent + '  '))
                result.append(indent + '}')
            else:
                result.append(indent + '}')
                result.append(indent + 'if (z[{}]==0) {{'.format(number))
                result.extend(probe(number, 'false', indent + '  '))
                result.append(indent + '}')
            index = following
        return result

    transformed = '\n'.join(block(0, len(lines))) + '\n'
    if not decisions:
        raise ValueError('library has no recognized decisions')
    return transformed, sorted(decisions, key=lambda item: item['line'])


def strip_probes(output):
    """Return original output and the set of observed (line, outcome) pairs."""
    plain, outcomes = [], set()
    for line in output.replace('\\\r\n', '').replace('\\\n', '').splitlines():
        if line.startswith(MARKER):
            number, outcome, zero = line[len(MARKER):].split()
            if outcome not in ('true', 'false') or zero != '0':
                raise ValueError('invalid interpreted-decision probe')
            outcomes.add((int(number), outcome))
        else:
            plain.append(line)
    return plain, outcomes
