#!/usr/bin/env python3
"""Reject a qualification declaration beyond standard hosted job limits."""
from pathlib import Path
import re
import unittest

ROOT=Path(__file__).resolve().parents[2]

class HostedLimits(unittest.TestCase):
    def test_declared_job_fits_standard_hosted_limit_and_retention_buffer(self):
        text=(ROOT/'.github/workflows/bc-reconstruction-qualification.yml').read_text()
        job=int(re.search(r'^    timeout-minutes: ([0-9]+)$',text,re.M).group(1))
        total=int(re.search(r'commands=deadline.Commands\(driver,([0-9]+)\)',text).group(1))
        self.assertLessEqual(job,360,'standard GitHub-hosted jobs have a six-hour limit')
        self.assertLessEqual(total+900,job*60,'retain time for source and original artifact uploads')
        nested=[int(value) for value in re.findall(r"'--total-seconds','([0-9]+)'",text)]
        self.assertTrue(nested)
        self.assertLessEqual(max(nested)+600,total,'retain inner controller evidence before outer termination')

if __name__=='__main__':unittest.main()
