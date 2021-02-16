#include "util/file.hh"

#ifdef _WIN32
#  include <fcntl.h>
#  include <io.h>
#  include <stdio.h>
#endif

void GD::make_stdin_binary()
{
#ifdef _WIN32
  _setmode(_fileno(stdin), _O_BINARY);
#endif  // _WIN32
}
