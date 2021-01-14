# Standard Supplement Library (libgdsup)

The aim of this library is to make sure all the utilities can build against a standard set of APIs.

Unfortunately, this is necessary because we are targeting multiple platforms and compilers.

The goal is to provide Standards (C, C++, POSIX) compliant interfaces for all the functions we need.
The goal is not to be complete.  In some cases we will use third-party libraries to provide that
functionality for us.

We only use the support library where we need to and rely on platform implementations as much as
possible.

## Configuration

The build system will configure the support library automatically.  To force the use of the support
library functionality even if not technically needed specify `-DFORCE_SUPPLEMENTAL_LIBRARY:BOOL=ON`
on the `cmake` configure line.

Sources are in the [libgdsup/](../../libgdsup) directory.

## Adding to applications

Add the library target `gdsup` as a dependency of your application to get the library included,
along with the correct include paths.

### Includes

To expose functionality in you C/C++ source do:

```c
#include "gd/FOO.hh" // For a C++ standard header <FOO>
#include "gd/BAR.h"  // For a C/POSIX standard header <BAR.H>
```

These will redirect to the appropriate standard header if necessary.

## Further Documentation

 * [Header file detail](./header-files.md)
 * [Limits](./limits.md)
 * [Windows Support](./windows-support.md)
