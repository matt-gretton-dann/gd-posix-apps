/** \file libgdsup/string/strdup.c
 *  \brief Implemenation of strdup()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/string.h"

#include <errno.h>

char* strdup(char const* s)
{
  char* result = _strdup(s);
  if (result == NULL) {
    errno = ENOMEM;
  }
  return result;
}
