#!/usr/bin/env python3
"""Check component oracles and boundaries without asserting noisy durations."""
import argparse
import json
import subprocess
import sys
import unittest


class Components(unittest.TestCase):
    def invoke(self, component, source, iterations='3', split='2', success=True, base=None):
        argv = [DRIVER, component, iterations, split]
        if base is not None:
            argv.append(str(base))
        child = subprocess.run(argv, input=source,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               universal_newlines=True, timeout=5)
        if not success:
            self.assertNotEqual(child.returncode, 0)
            self.assertTrue(child.stderr)
            return None
        self.assertEqual(child.returncode, 0, child.stderr)
        self.assertEqual(child.stderr, '')
        result = json.loads(child.stdout)
        self.assertEqual(result['iterations'], int(iterations))
        self.assertEqual(result['split_point'], int(split))
        self.assertEqual(result['text_base'], 10 if base is None else int(base))
        if result['instrumented']:
            self.assertIsNone(result['seconds'])
        else:
            self.assertGreaterEqual(result['seconds'], 0)
        return result

    def test_arithmetic_exact_oracles(self):
        left = 123456789012345678901234567890
        right = 998877665544332211
        for split in ('1', '2', '126'):
            for component, expected in (('control', left), ('add', left + right),
                                        ('subtract', left - right), ('multiply', left * right),
                                        ('square', left * left), ('divide', left // right)):
                with self.subTest(component=component, split=split):
                    result = self.invoke(component, '{}\n{}\n'.format(left, right), split=split)
                    self.assertEqual(result['result'], str(expected))
                    self.assertEqual(result['checksum_mod_2_64'], 3 * len(str(expected)))

    def test_prepared_coefficient_oracles(self):
        for left, right in ((0, 97), (96, 97), (97, 97),
                            (10 ** 279 + 123456789, 10 ** 134 + 987654321),
                            (10 ** 199 - 1, 9 * 10 ** 89 + 314159)):
            for component, expected in (
                    ('coefficient-control', left), ('prepare-divisor', right),
                    ('coefficient-divide', left // right),
                    ('prepared-divide', left // right),
                    ('coefficient-modulo', left % right),
                    ('prepared-modulo', left % right)):
                with self.subTest(component=component, left=left, right=right):
                    result = self.invoke(component, f'{left}\n{right}\n')
                    self.assertEqual(result['result'], str(expected))
                    self.assertEqual(result['consumer'], 'stored_coefficient_limbs')
                    if result['instrumented']:
                        self.assertIsNone(result['setup_seconds'])
                        self.assertGreaterEqual(result['operation_allocation_calls'], 0)
                        self.assertGreaterEqual(result['operation_allocated_bytes'], 0)
                    else:
                        self.assertGreaterEqual(result['setup_seconds'], 0)

    def test_coefficient_protocol_rejections(self):
        for component in ('coefficient-divide', 'prepared-divide', 'prepare-divisor'):
            for source in ('1\n0\n', '-1\n2\n', '1.5\n2\n', '1\n',
                           '1\n2\n3\n', '1\n2x\n'):
                with self.subTest(component=component, source=source):
                    self.invoke(component, source, success=False)
            self.invoke(component, '1\n2\n', base=10, success=False)
        self.invoke('prepared-unknown', '1\n2\n', success=False)

    def test_signed_fractional_arithmetic(self):
        for component, expected in (('add', '-11.00'), ('subtract', '-13.50'),
                                    ('multiply', '-15.31'), ('divide', '-9')):
            result = self.invoke(component, '-12.25\n1.25\n')
            self.assertEqual(result['result'], expected)

    def test_conversion_and_continuation_lines(self):
        literal = '1234567890' * 40
        for component in ('conversion-in', 'conversion-out'):
            result = self.invoke(component, literal + '\n')
            self.assertEqual(result['result'], literal)
            if component == 'conversion-in':
                self.assertEqual(result['checksum_mod_2_64'], len(literal) * 3)
            else:
                self.assertGreaterEqual(result['checksum_mod_2_64'], len(literal) * 3)

    def test_nondecimal_conversion_oracles(self):
        alphabet = '0123456789ABCDEF'
        value = 123456789012345678901234567890
        def encode(number, base):
            result = ''
            while number:
                number, remainder = divmod(number, base)
                result = alphabet[remainder] + result
            return result or '0'
        for base in range(2, 17):
            with self.subTest(base=base):
                digits = encode(value, base)
                decoded = self.invoke('conversion-in', digits + '\n', base=base)
                self.assertEqual(decoded['result'], str(value))
                self.assertEqual(decoded['checksum_mod_2_64'], len(str(value)) * 3)
                encoded = self.invoke('conversion-out', str(value) + '\n', base=base)
                self.assertEqual(encoded['result'], digits)
        self.assertEqual(self.invoke('conversion-in', 'F\n', base=2)['result'], '15')
        self.assertEqual(self.invoke('conversion-in', '1.1\n', base=2)['result'], '1.5')
        self.assertEqual(self.invoke('conversion-in', '-A.F\n', base=16)['result'], '-10.9')
        self.assertEqual(self.invoke('conversion-out', '1.5\n', base=2)['result'], '1.1000')

    def test_divide_conquer_integer_conversion_protocol(self):
        alphabet = '0123456789ABCDEF'
        value = 10 ** 79 + 123456789012345678901234567890
        def encode(number, base):
            result = ''
            while number:
                number, remainder = divmod(number, base)
                result = alphabet[remainder] + result
            return result or '0'
        for base in range(2, 17):
            with self.subTest(base=base):
                digits = encode(value, base)
                parsed = self.invoke('conversion-in-dc', '000' + digits + '\n', base=base)
                self.assertEqual(parsed['result'], str(value))
                self.assertEqual(parsed['checksum_mod_2_64'], 3 * len(str(value)))
                output = self.invoke('conversion-out-dc', str(value) + '\n', base=base)
                self.assertEqual(output['result'], digits)
                wraps = len(digits) // (69 if len(digits) <= 69 else 68) if base == 10 else (
                    0 if len(digits) <= 69 else (len(digits) - 1) // 68)
                self.assertEqual(output['checksum_mod_2_64'], 3 * (len(digits) + 2 * wraps))
        self.assertEqual(self.invoke('conversion-in-dc', 'F\n', base=2)['result'], '15')
        for component in ('conversion-in-dc', 'conversion-out-dc'):
            self.assertEqual(self.invoke(component, '0\n', base=2)['result'], '0')

    def test_divide_conquer_boundaries_and_zero_remainders(self):
        alphabet = '0123456789ABCDEF'
        for base in (2, 3, 8, 16):
            for width in (64, 65, 127, 128, 1024):
                with self.subTest(base=base, width=width):
                    digits = alphabet[base - 1] * width
                    decimal = str(int(digits, base))
                    parsed = self.invoke('conversion-in-dc', digits + '\n', base=base)
                    self.assertEqual(parsed['result'], decimal)
                    self.assertEqual(parsed['checksum_mod_2_64'], 3 * len(decimal))
                    output = self.invoke('conversion-out-dc', decimal + '\n', base=base)
                    self.assertEqual(output['result'], digits)
            for exponent in (64, 128, 256):
                with self.subTest(base=base, exponent=exponent):
                    value = str(base ** exponent)
                    output = self.invoke('conversion-out-dc', value + '\n', base=base)
                    self.assertEqual(output['result'], '1' + '0' * exponent)
            self.assertEqual(self.invoke('conversion-in-dc', '0' * 128 + '1\n',
                                         base=base)['result'], '1')

    def test_divide_conquer_diagnostics_reject_noninteger_fixtures(self):
        for component in ('conversion-in-dc', 'conversion-out-dc'):
            for source in ('-1\n', '1.5\n', '+1\n'):
                with self.subTest(component=component, source=source):
                    self.invoke(component, source, success=False, base=10)

    def test_nondecimal_conversion_rejects_invalid_bases_and_digits(self):
        for component, source, base in (('conversion-in', '1\n', 1),
                                        ('conversion-out', '1\n', 17),
                                        ('conversion-in', '12\n', 2),
                                        ('conversion-in', 'FG\n', 16),
                                        ('conversion-in', '1..1\n', 2),
                                        ('conversion-in', 'A\n', 10),
                                        ('add', '1\n2\n', 2),
                                        ('parse', '1\n', 2)):
            with self.subTest(component=component, source=source, base=base):
                self.invoke(component, source, base=base, success=False)

    def test_parse_performs_no_execution(self):
        one = self.invoke('parse', '1/0\n')
        two = self.invoke('parse', '1/0\n2/0\n')
        self.assertEqual(one['result'], '')
        self.assertGreater(one['instructions'], 0)
        self.assertGreater(two['instructions'], one['instructions'])
        self.assertEqual(one['checksum_mod_2_64'], one['instructions'] * 3)

    def test_parse_deep_right_associative_power(self):
        powers = 131072
        result = self.invoke('parse', '1^' * powers + '1\n', iterations='1')
        self.assertEqual(result['instructions'], 2 * powers + 3)

    def test_invalid_boundaries(self):
        for component, source, count, split in (
                ('unknown', '1\n', '1', '2'), ('add', '1\n', '0', '2'),
                ('add', '1\n', '2x', '2'), ('add', '1\n', '1', '0'),
                ('add', '1\n', '1', '1000000001'), ('add', '12x\n', '1', '2'),
                ('add', '.\n', '1', '2'), ('add', '1\n', '1', '2'),
                ('control', '1\n2\n3\n', '1', '2'), ('divide', '1\n0\n', '1', '2'),
                ('parse', 'quit\n', '1', '2'), ('add', '1' * (1024 * 1024 + 1), '1', '2')):
            with self.subTest(component=component, count=count, split=split):
                self.invoke(component, source, count, split, success=False)


    def test_parse_deep_assignment_and_negation(self):
        for prefix, count, expected in (('a=', 65536, 131074), ('- ', 131072, 131075)):
            with self.subTest(prefix=prefix):
                result = self.invoke('parse', prefix * count + '1\n', iterations='1')
                self.assertEqual(result['instructions'], expected)


    def test_parse_deep_nested_statements(self):
        depth = 65536
        for name, source, count in (
                ('if', 'if (1) ' * depth + '1\n', 2 * depth + 3),
                ('block', '{\n' * depth + '1\n' + '}\n' * depth, 3)):
            with self.subTest(shape=name):
                result = self.invoke('parse', source, iterations='1')
                self.assertEqual(result['instructions'], count)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('driver')
    args, remaining = parser.parse_known_args()
    DRIVER = args.driver
    unittest.main(argv=[__file__, *remaining])
