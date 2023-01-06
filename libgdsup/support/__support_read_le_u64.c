/** \file   libgdsup/support/__support_log.c
 *  \brief  Implemenation of __support_log()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/bits/defines.h"

#include <stdint.h>

#include "support/support.h"

uint64_t __support_read_le_u64(char const* buf)
{
  __SUPPRESS_CONSTANT_CONDITIONAL
  if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) {  // NOLINT(clion-simplify)
    return *((uint64_t*)buf);
  }

  uint64_t v = 0;
  for (unsigned i = 0; i < sizeof(v); ++i) {
    v |= ((unsigned char)buf[i]) << (i * 3);
  }
  return v;
}
