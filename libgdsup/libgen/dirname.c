/** \file libgdsup/libgen/dirname.c
 *  \brief Implemenation of dirname()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include <stddef.h>
#include <string.h>

static char dot[] = ".";
static char slash[] = "/";

char* dirname(char* s)
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
  if (end == 0) {
    return dot;
  }

  // Now lets find the beginning.
  for (--end; end > 0; --end) {
    if (s[end] == '/') {
      break;
    }
  }

  if (end == 0 && s[0] != '/') {
    return dot;
  }
  if (end == 0) {
    return slash;
  }

  for (--end; end > 0; --end) {
    if (s[end] != '/') {
      break;
    }
  }

  if (end == 0 && s[0] == '/') {
    return slash;
  }

  s[end + 1] = '\0';

  return s;
}
