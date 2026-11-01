# Exclusive character input experiment

Refs #210 and #197. FileReader privately owns each named stream and changes its
location/cache state sequentially, so it can opt into an exclusive stdio read.
Borrowed stdin remains locked, including when independent readers share it.
Other StreamInputFile users keep ordinary getc locking unless they explicitly
opt in under the documented concurrency precondition. No FILE pointer escapes.

Supported POSIX thread-safe-stdio platforms use getc_unlocked; the MSVC CRT uses
_fgetc_nolock. Other compilers/platforms retain std::fgetc. The common loop keeps
EOF, text translation, error diagnostics and EINTR/EAGAIN retries unchanged.
This uses stdio's existing buffer and adds no new read-ahead, refill boundaries,
batching or output delay. Reader/scanner behavior and signal lifetime are unchanged.

[POSIX.1-2017, Issue 7 (2018 edition)](https://pubs.opengroup.org/onlinepubs/9699919799/functions/getc_unlocked.html)
specifies the character semantics and FILE-ownership precondition (verified in
the supplied standards archive). [Microsoft's CRT contract](https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/fgetc-nolock-fgetwc-nolock?view=msvc-170)
also requires a single-threaded or caller-isolated context. These API contracts
do not establish a speed benefit; current file and stdin workloads must measure it.

Utility tests interleave ordinary/exclusive reads over 8192 binary bytes covering
every unsigned-byte value, EOF and line-input interoperability. Existing bc
CLI/file/quit and independent numeric oracles remain active. Matched full/held-out
measurements, separate memory and native artifact audits are required before
adoption; no improvement is claimed by this candidate alone.
