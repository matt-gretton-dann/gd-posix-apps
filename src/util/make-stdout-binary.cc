#include "util/file.hh"

#ifdef _WIN32
#  include <fcntl.h>
#  include <io.h>
#  include <stdio.h>
#endif

void GD::make_stdout_binary()
{
#ifdef _WIN32
  _setmode(_fileno(stdout), _O_BINARY);
#endif  // _WIN32
}
