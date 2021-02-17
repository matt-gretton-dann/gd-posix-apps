/**
 * \file      src/util/rename.cc
 * \brief     Wrapper around rename()
 * \author    Matthew Gretton-Dann
 * \copyright 2021, Matthew Gretton-Dann
 *            SPDX-License-Identifier: Apache-2.0
 */
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "util-messages.hh"

#include <errno.h>
#include <iostream>
#include <stdio.h>

#include "util-internals.hh"

bool GD::rename(std::string const& from, std::string const& to)
{
  int r = ::rename(from.data(), to.data());
  if (r == -1) {
    Util::message(Util::Msg::file_rename_error, from, to, errno, ::strerror(errno));
  }

  return r == 0;
}
