/** \file   src/util/input-file/input-file-report-error.cc
 *  \brief  Report an error
 *  \author Copyright 2021, Matthew Grett-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/string.h"

#include "util/file.hh"
#include "util/utils.hh"

#include "util-messages.hh"

#include <errno.h>
#include <iostream>

void GD::StreamInputFile::report_error(GD::Util::Msg msg)
{
  std::cerr << GD::program_name() << ": "
            << GD::Util::Messages::get().format(GD::Util::Set::util, msg, filename_, errno,
                                                ::strerror(errno))
            << '\n';
}
