# Combined input and decimal-formatting validation

Exact combined source 03f6ffe5982866b7757b538e100b9f985d64fb1e merges accepted
main dd8c60c (PR #338) into the formatter topic. Its merge commit also archives
PR #338 native artifacts. The archive-only wording in that commit body does
not describe the integration: the branch runtime now combines balanced large
input and fixed-buffer decimal output. No timings from the earlier d0947b5
candidate are relabeled as this combined source.

Release passes 202 cases/691634 assertions and eight CLI groups. Extensions-OFF
ASan/UBSan passes 29 focused radix/input/output cases/37681 assertions, fifteen
VM CLI and ten component groups. Twelve profiling CLI groups pass. The focused
Catch command also names radix-decode, a tag absent from this topic; Catch
reports that absence explicitly, and the 29 matched cases still pass. Byte-table
decoding belongs to a separate evaluation and is not part of this source.

Adjacent locale/map includes and conversion guide additions were reconciled;
source behavior from each topic remains covered. Fresh three-platform native
checks at the final PR head are required before merge. Refs #206 and #197.
