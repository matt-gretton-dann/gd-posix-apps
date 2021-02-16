#include "util/file.hh"

#include "util-messages.hh"

#include <errno.h>
#include <string>
#include <string_view>
#include <utility>

#include "util-internals.hh"

std::pair<int, std::string> GD::mkstemp(std::string_view base)
{
  std::string fname = std::string(base) + ".XXXXXX";
  int fd = ::mkstemp(fname.data());
  if (fd == -1) {
    Util::message(Util::Msg::file_open_error, fname, errno, ::strerror(errno));
  }

  return std::make_pair(fd, fname);
}
