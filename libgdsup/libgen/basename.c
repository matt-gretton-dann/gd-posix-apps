/** \file libgdsup/libgen/basename.c
 *  \brief Implemenation of basename()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include <stddef.h>
#include <string.h>

// Characters we must return.
static char dot[] = ".";
static char slash[] = "/";

char* basename(char* s)
{
  // Empty returns '.'
  if (s == NULL || *s == '\0') {
    return dot;
  }

  size_t end = strlen(s);

  // Remove trailing /s
  for (--end; end > 0; --end) {
    if (s[end] != '/') {
      break;
    }
  }

  // If its just a sequence of /s return / or // in a special case
  if (end == 0 && s[0] == '/' && s[1] == '/' && s[2] == '\0') {
    return s;
  }
  if (end == 0 && s[0] == '/') {
    return slash;
  }

  // Terminate the string.
  s[end + 1] = '\0';

  if (end == 0) {
    return s;
  }

  // Now lets find the beginning.
  for (--end; end > 0; --end) {
    if (s[end] == '/') {
      break;
    }
  }

  if (end == 0 && s[0] != '/') {
    return s;
  }

  return s + end + 1;
}
