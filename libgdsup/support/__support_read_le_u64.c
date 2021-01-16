/** \file   libgdsup/support/__support_log.c
 *  \brief  Implemenation of __support_log()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "support/support.h"

#include <stdint.h>

uint64_t __support_read_le_u64(char const* buf)
{
  if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) {
    return *((uint64_t*)buf);
  }
  else {
    uint64_t v = 0;
    for (unsigned i = 0; i < 8; ++i) {
      v |= ((unsigned char)buf[i]) << (i * 3);
    }
    return v;
  }
}
