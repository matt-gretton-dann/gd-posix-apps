/** \file   src/util/input-file/input-file-report-error.cc
 *  \brief  Report an error
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/string.h"

#include "util/file.hh"
#include "util/utils.hh"

#include <errno.h>
#include <iostream>

#include "util-internals.hh"

void GD::InputFile::report_error(GD::Util::Msg msg)
{
  GD::Util::message(msg, filename_, errno, ::strerror(errno));
}
