# Accepted large-input runtime conversion

PR #338 accepts exact source 75de1ad against 9669ef7. Native run 41295439584
has 1650 checked samples per OS and no confirmed regressions. Original ZIPs
retain exact recipes, compiler/library/binary identities, rows and every advisory
interval; audit.json records verified SHA-256 values and identities. All fourteen
head checks, including both CodeQL runs, passed before merge dd8c60c. The root
checkout immediately fast-forwarded to origin/main after that merge.

Binary input at 100000 characters (three separate source literals) measures
0.350/0.505/0.474 of baseline on Linux/macOS/Windows; hexadecimal measures
0.127/0.170/0.170. These are end-to-end input workloads, distinct from local
component timings. Inconclusive outcomes remain in the full advisory tables.
Refs #206, #216 and #197; these issues retain their remaining work.
