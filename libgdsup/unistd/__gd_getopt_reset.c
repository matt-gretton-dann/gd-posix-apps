/** \file   libgdsup/unistd/__gd_getopt_reset.c
 *  \brief  Implement __gd_getopt_reset()
 *  \author Copyright 2020-2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */
#include "gd/bits/defines.h"
#include "gd/stdlib.h"
#include "gd/unistd.h"

void __gd_getopt_reset(void) __NOEXCEPT
{
  optarg = NULL;
  opterr = 1;
  optind = 1;
  optopt = 0;
}
