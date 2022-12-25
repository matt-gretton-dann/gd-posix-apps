# /usr/bin/env python3
#
# Run clang-format sucking all standard output to /dev/null
# Command line arguments are how to invoke clang-format
# Exit code is the exit code from clang-format (or 127 if we can't duplicate
# it)
import subprocess
import sys

# Run the command we were given
cp = subprocess.run(sys.argv[1:], stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    universal_newlines=True)

if cp.returncode == 0:
    # Run succeeded - we need to compare the output with the input file:
    source_file = sys.argv[-1]
    with open(source_file, 'r') as sf:
        sft = sf.read()
        if cp.stdout == sft:
            exit(0)
        else:
            sys.stderr.write(
                'clang-format-check: file needs formatting - {}\n'.format(
                    source_file))
            exit(1)
else:
    # Report an error
    sys.stderr.write('clang-format-check: clang-format error running: ')
    sys.stderr.write(' '.join(sys.argv[1:]) + '\n')
    sys.stderr.write(cp.stderr.decode("utf-8"))

    if cp.returncode > 0:
        exit(cp.returncode)
    else:
        # This will have been a signal on POSIX.
        exit(127)
