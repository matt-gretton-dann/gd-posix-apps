/**
 * \file      mkstemp.cc
 * \brief     Wrapper around ::mkstemp()
 * \author    Matthew Gretton-Dann
 * \copyright 2021 Matthew Gretton-Dann
 *            SPDX-License-Identifier: Apache-2.0
 */
#include "gd/stdlib.h"

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
