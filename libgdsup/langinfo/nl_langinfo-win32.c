/**
 * \file      libgdsup/langinfo/nl_langinfo.c
 * \brief     Implement nl_langinfo() on platforms that don't support it (windows).
 * \author    Matthew Gretton-Dann
 * \copyright 2021, Matthew Gretton-Dann
 *            SPDX-License-Identifier: Apache-2.0
 */

#include "gd/langinfo.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

/* For Windows we currently insist of POSIX locale responses.  */
char* nl_langinfo(nl_item __item)
{
  static char yesexpr[] = "^[Yy]";
  static char noexpr[] = "^[Nn]";

  switch (__item) {
  case YESEXPR:
    return yesexpr;
  case NOEXPR:
    return noexpr;
  default:
    return NULL;
  }
}
