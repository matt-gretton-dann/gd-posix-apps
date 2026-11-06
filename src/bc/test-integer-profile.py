#!/usr/bin/env python3
"""Prove diagnostic builds execute and account for the selected integer tier."""
import argparse
import json
import math
import os
import random
from pathlib import Path
import subprocess
import tempfile
import unittest
import sys


class IntegerProfile(unittest.TestCase):
    def profile(self, text, expected, unfold=False):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / 'integer-profile.json'
            environment = {k: v for k, v in os.environ.items()
                           if not k.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
            environment.update(LC_ALL='C', GD_BC_PROFILE_OUTPUT=str(output))
            run = subprocess.run([BC], input=text, universal_newlines=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 timeout=10, env=environment)
            # Numeric oracles compare complete decimal values. Long-number
            # continuation formatting has separate CLI regression coverage.
            stdout = run.stdout.replace('\\\n', '') if unfold else run.stdout
            self.assertEqual((run.returncode, stdout, run.stderr), (0, expected, ''))
            data = json.loads(output.read_text())
        self.assertTrue(data['instrumented'])
        self.assertEqual(data['total']['live_bytes'], 0)
        for allocation in data['allocations'].values():
            self.assertEqual(allocation['live_bytes'], 0)
        return data

    def checked_hot_function(self, definition, inputs, oracle):
        # Warm the generation before any asserted input. Event assertions make
        # unsupported-plan fallback an explicit failure of this engine test.
        program = definition + 'for(i=0;i<12;i++) q=f(0)\n'
        program += ''.join('f({})\n'.format(value) for value in inputs)
        expected = ''.join('{}\n'.format(oracle(value)) for value in inputs)
        data = self.profile(program, expected, unfold=True)
        events = data['events']
        self.assertGreaterEqual(events['integer_function_successes'], len(inputs))
        self.assertEqual(events['integer_function_attempts'], events['integer_function_successes'])
        self.assertEqual(events['integer_function_abandonments'], 0)
        return events

    def test_generated_signed_control_flow_matches_independent_integers(self):
        inputs = [0, 1, -1, 9, -9, 2 ** 31, -(2 ** 31),
                  2 ** 63 - 1, -(2 ** 63), 2 ** 62, -(2 ** 62)]
        generator = random.Random(598612)
        for case in range(16):
            d, e = generator.randrange(10), generator.randrange(10)
            with self.subTest(case=case, d=d, e=e):
                definition = ('define f(x) {\nauto a,b,c,k,t\na=x\nb= -x\n'
                              'if(x<0) {\na=a*b\nb=b+D\n}\n'
                              'if(0<=x) {\nb=b*a\na=a-D\n}\n'
                              'for(k=0;k<3;k++) {\nc=a\nt=0\nif(a<=b) t=1\n'
                              'if(t) a=b-a\nif(t==0) a=a+b\nb=c*(x+E)\n}\n'
                              'return (a-b)\n}\n').replace('D', str(d)).replace('E', str(e))

                def oracle(x):
                    a, b = x, -x
                    if x < 0:
                        a, b = a * b, b + d
                    else:
                        b, a = b * a, a - d
                    for _ in range(3):
                        c = a
                        a = b - a if a <= b else a + b
                        b = c * (x + e)
                    return a - b

                events = self.checked_hot_function(definition, inputs, oracle)
                self.assertGreater(events['integer_function_promotions'], 0)

    def test_generated_recursive_products_match_independent_integers(self):
        generator = random.Random(598613)
        for case in range(8):
            d, e = generator.randrange(10), generator.randrange(2, 10)
            with self.subTest(case=case, d=d, e=e):
                definition = ('define f(x) {\nauto a,b\nif(x<=1) return (x+E)\n'
                              'a=f(x-1)\nb=f(x-2)\nreturn (a*b-D)\n}\n')
                definition = definition.replace('D', str(d)).replace('E', str(e))
                values = [e, e + 1]
                for _ in range(2, 11):
                    values.append(values[-1] * values[-2] - d)
                events = self.checked_hot_function(definition, list(range(11)), values.__getitem__)
                self.assertGreater(events['integer_function_self_calls'], 0)

    def test_nested_self_call_arguments_keep_outer_marks(self):
        definition = ('define f(x) {\nif(x<=1) return (1)\n'
                      'return (f(x-1)+f(f(0)))\n}\n')
        events = self.checked_hot_function(definition, list(range(13)), lambda x: max(1, x))
        self.assertGreater(events['integer_function_self_calls'], 0)

    def test_hot_calls_execute_the_selected_tier(self):
        data = self.profile('define f(x) {\nreturn (x*x+1)\n}\n'
                            's=0\nfor(i=0;i<20;i++) s+=f(i)\ns\n',
                            str(sum(i * i + 1 for i in range(20))) + '\n')
        events = data['events']
        self.assertGreater(events.get('integer_function_attempts', 0), 0)
        self.assertEqual(events['integer_function_successes'], events['integer_function_attempts'])
        self.assertEqual(events['integer_function_abandonments'], 0)
        self.assertGreater(events['integer_function_instructions'], 0)
        self.assertEqual(events['integer_function_promotions'], 0)
        self.assertGreater(data['allocations']['frames']['allocated_bytes'], 0)
        self.assertGreater(data['allocations']['execution_plans']['allocated_bytes'], 0)

    def test_exact_large_results_report_promotions_and_self_calls(self):
        data = self.profile('define f(x) {\nif(x<=1) return (1)\nreturn (x*f(x-1))\n}\n'
                            'for(i=0;i<12;i++) a=f(50)\na\n', str(math.factorial(50)) + '\n')
        events = data['events']
        self.assertGreater(events.get('integer_function_successes', 0), 0)
        self.assertGreater(events['integer_function_self_calls'], 0)
        self.assertGreater(events['integer_function_promotions'], 0)
        self.assertEqual(events['integer_function_abandonments'], 0)

    def test_cold_and_hot_zero_results_accept_unsigned_boundaries(self):
        # The first call uses the generic interpreter; later calls must
        # actually use the integer tier and preserve the same zero contract.
        data = self.profile('define f(x) {\nreturn (-x)\n}\nscale=f(0)\n'
                            'for(i=0;i<12;i++) q=f(0)\nscale=f(0)\n'
                            'a[f(0)]=7\nscale\na[0]\n', '0\n7\n')
        self.assertGreater(data['events']['integer_function_successes'], 0)
        self.assertEqual(data['events']['integer_function_abandonments'], 0)

    def test_escaped_results_survive_later_calls_and_redefinition(self):
        large = 2 ** 63 - 1
        data = self.profile('define f(x) {\nreturn (x*x)\n}\n'
                            'define g(x) {\nreturn (x*x*x)\n}\n'
                            'for(i=0;i<12;i++) {\nq=f(2)\nq=g(2)\n}\n'
                            f'a=f({large})\nb=g(-{large})\nq=f(3)\n'
                            'scale=1\nq=f(2)\nscale=0\n'
                            'define g(x) {\nreturn (x+1)\n}\n'
                            'for(i=0;i<12;i++) q=g(2)\nq=g(3)\na\nb\nq\n',
                            f'{large ** 2}\n{-large ** 3}\n4\n', unfold=True)
        self.assertGreaterEqual(data['events']['integer_function_successes'], 4)
        self.assertGreater(data['events']['integer_function_promotions'], 0)
        self.assertEqual(data['events']['integer_function_abandonments'], 0)

    def test_effectful_functions_remain_on_the_generic_path(self):
        data = self.profile('define f(x) {\ns=s+1\nreturn (x)\n}\n'
                            'for(i=0;i<20;i++) a=f(i)\ns\n', '20\n')
        self.assertEqual(data['events'].get('integer_function_attempts', 0), 0)

    def test_redefinition_discards_a_previously_eligible_plan(self):
        data = self.profile('define f(x) {\nreturn (x*x+1)\n}\n'
                            'for(i=0;i<20;i++) a=f(i)\n'
                            'define f(x) {\ns=s+1\nreturn (x)\n}\n'
                            'for(i=0;i<20;i++) a=f(i)\ns\n', '20\n')
        self.assertGreater(data['events']['integer_function_successes'], 0)

    def test_redefinition_discards_a_previously_rejected_plan(self):
        data = self.profile('define f(x) {\ns=s+1\nreturn (x)\n}\n'
                            'for(i=0;i<20;i++) a=f(i)\n'
                            'define f(x) {\nreturn (x*x+1)\n}\n'
                            'for(i=0;i<20;i++) a=f(i)\ns\na\n', '20\n362\n')
        self.assertGreater(data['events']['integer_function_successes'], 0)

    def test_cancellation_reports_demotion(self):
        data = self.profile('define f(x) {\nauto a\na=x*x\nreturn (a-a)\n}\n'
                            'for(i=0;i<12;i++) a=f(9223372036854775807)\na\n', '0\n')
        self.assertGreater(data['events']['integer_function_promotions'], 0)
        self.assertGreater(data['events']['integer_function_demotions'], 0)
        self.assertEqual(data['events']['integer_function_abandonments'], 0)

    def test_size_guard_reports_abandonment_and_reuses_generic_fallback(self):
        if hasattr(sys, 'set_int_max_str_digits'):
            sys.set_int_max_str_digits(0)
        expected = str(len(str(9 ** (2 ** 14)))) + '\n'
        data = self.profile('define f(x) {\nauto y\ny=9\n'
                            'while(x>0) {\ny=y*y\nx=x-1\n}\nreturn (y)\n}\n'
                            'for(i=0;i<12;i++) a=f(14)\nlength(a)\n', expected)
        self.assertGreater(data['events']['integer_function_abandonments'], 0)
        self.assertEqual(data['events']['integer_function_successes'], 0)
        self.assertEqual(data['events']['integer_function_attempts'],
                         data['events']['integer_function_abandonments'])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    args, remaining = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remaining])
