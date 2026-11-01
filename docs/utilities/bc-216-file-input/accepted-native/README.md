# Accepted file-input transport and native class

Refs #216, #210 and #197. PR #348 accepts exact candidate
`2819b02d55836f935f94aa0bf2a47205efa2d9a3` against native base
`a721a1a85f9dc5947836741787904f8e7952a4a8`. All fourteen checks pass.
This changes benchmark infrastructure, not production C++; it makes no scanner
speed or disk-throughput claim. The three artifact digests and source identities
remain in audit.json and the unmodified native ZIPs.

Each OS contains 1750 checked A/A and paired invocations, 5250 total, with no
confirmed regression. Each run includes explicit file mode, EOF on stdin, and
independent exact final values. All 140 staged program/oracle hashes per artifact
were verified (420 across the three), alongside exact source/harness identity.
Windows checks out the Python harness with CRLF; Linux/macOS use LF. Both raw
byte hashes are retained, not relabeled. Generated program/oracle artifacts remain
exact LF bytes on every host.

File candidate/base ratios: Linux 0.997 [0.972,1.010], macOS 1.038
[0.869,1.386] inconclusive and Windows 0.987 [0.871,1.015]. Other noisy/lower
confidence classes remain in complete summaries. No universal neutrality is
claimed from these shared runners; correctness/builds/deadlines remain hard gates.

Twenty-two harness cases and six native-policy cases pass on Python 3.9 and
3.15, including native quick execution of all 46 workloads. Test-first logs show
the missing class/transport before implementation. The adjacent corpus proof
retains every prior dictionary and program/oracle hash across all three presets.
Broader calibration and math classes remain #216 work; scanner and I/O policy
remain #210 work.
