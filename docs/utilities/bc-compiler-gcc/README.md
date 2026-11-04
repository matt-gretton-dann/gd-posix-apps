# Linux GCC optimization matrix

Refs #213. Run the checked full and held-out arithmetic corpora with
matched GCC O2 and O3 builds, each with IPO disabled and required. All
four variants use the same checkout, native GCC toolchain, Release
mode, extensions and a multiplication split point of 126. A pinned
Howard build uses the same GCC C compiler.

```sh
bash docs/utilities/bc-compiler-gcc/run.sh . /path/to/new-output
```

The runner checks the actual optimization and IPO flags in each VM
compile command and runs the CLI suite before timing. It retains
compiler versions, revision and binary identities, compile commands,
build logs, checked samples and paired summaries. The build logs give
single wall-time observations. Results from the hosted runner are
advisory and have no separate A/A control.
