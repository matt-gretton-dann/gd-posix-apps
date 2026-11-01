# Initial grouped-input candidate remains on hold

Native run 41284692434 compares exact accepted main 3232d2f with source
13e44f6. All checked outputs/status/deadlines pass: Linux 1570, macOS and
Windows 1450 each. Digests and full source identities are in adjacent audits.

Linux confirms high/low/equal comparison ratios 1.081/1.096/1.095 and
base-changing literals 1.105. macOS and Windows have no confirmed losses.
These observations override neither local component gains nor semantics;
they put the production candidate on hold despite advisory green checks.
All raw samples and bounded confirmations remain in the ZIPs.

An out-of-line GCC text constructor is a bounded next candidate to isolate
long-form decoder code from surrounding inlining. This is a hypothesis, not
proof of the cause of the observed unrelated-class losses. New code requires
its own native evidence. Generic compiler policy elsewhere stays unchanged.
