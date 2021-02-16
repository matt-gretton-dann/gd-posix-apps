/** \file   src/util/unlink.cc
 *  \brief  Unlink a file.
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "gd/unistd.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "util-messages.hh"

#include <errno.h>
#include <iostream>
#include <stdio.h>

#include "util-internals.hh"

bool GD::unlink(std::string const& filename, bool report_errors)
{
  assert(filename != "-");
  int r = ::unlink(filename.data());
  if (r == -1 && report_errors) {
    GD::Util::message(GD::Util::Msg::file_unlink_error, filename, errno, ::strerror(errno));
  }

  return r == 0;
}
