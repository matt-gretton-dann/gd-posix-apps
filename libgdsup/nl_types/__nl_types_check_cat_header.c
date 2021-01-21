/** \file libgdsup/nl_types/__nl_types_check_cat_header.c
 *  \brief Implemenation of __nl_types_check_cat_header()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/limits.h"
#include "gd/nl_types.h"
#include "nl_types/nl_types.h"
#include "support/support.h"

#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>

bool __nl_types_check_cat_header(char const buffer[CAT_HDR_SIZE])
{
  /* Check the first 12 bytes of the header. */
  char expected[12] = {'M', 'S', 'G', '\0', '\1', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  for (unsigned i = 0; i < 12; ++i) {
    if (buffer[i] != expected[i]) {
      __support_log("Catalogue header invalid at byte %u: got %u expected %u\n", i,
                    (unsigned)buffer[i], (unsigned)expected[i]);
      return false;
    }
  }

  /* Check the size is in range. */
  uint64_t size = __support_read_le_u64(buffer + CAT_HDR_FILE_SIZE_OFFSET);
  if (size < CAT_HDR_SIZE || size > SIZE_MAX) {
    __support_log("Catalogue recorded size is invalid: got %" PRIu64 "u expected [24, %" PRIu64
                  "u]\n",
                  size, (uint64_t)SIZE_MAX);
    return false;
  }

  /* Check the number of sets is valid.  */
  uint32_t num_sets = __support_read_le_u32(buffer + CAT_HDR_NUM_SET_OFFSET);
  if (((uint64_t)num_sets) * CAT_ARRAY_ENTRY_SIZE > (size - CAT_HDR_SIZE)) {
    __support_log("Number of sets exceeds size of file: got %" PRIu32 "\n", num_sets);
    return false;
  }

  return true;
}
