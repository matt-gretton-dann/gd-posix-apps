/** \file   libgdsup/support/__support_log.c
 *  \brief  Implemenation of __support_log()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include <stdarg.h>
#include <stdio.h>

#include "support/support.h"

int __support_logging_enabled = 1;

void __support_log(char const* format, ...)
{
  if (__support_logging_enabled) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
  }
}
