/** \file   libgdsup/nl_types/test-catclose.cc
 *  \brief  Unit tests for catclose()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"
#include "nl_types/nl_types.h"

#include <catch2/catch.hpp>

TEST_CASE("__nl_types_check_cat_header", "[nl_types][nl_types_check_cat_header]")
{
  /* Check a correct buffer.  */
  char buf[24] = {'M',  'S',  'G',  '\0', '\1',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                  '\0', '\0', '\0', '\0', '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf) == true);

  /* Slight change in buffer.  */
  char buf2[24] = {'M',  's',  'G',  '\0', '\1',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                   '\0', '\0', '\0', '\0', '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf2) == false);

  /* Wrong version.  */
  char buf3[24] = {'M',  'S',  'G',  '\0', '\2',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                   '\0', '\0', '\0', '\0', '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf3) == false);

  /* Bad padding fields.  */
  char buf4[24] = {'M',  'S',  'G',  '\0', '\1',   '\0', '\0', 'B',  'A',  'D',  '\0', '\0',
                   '\0', '\0', '\0', '\0', '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf4) == false);

  /* File size too small.  */
  char buf5[24] = {'M',  'S',  'G',  '\0', '\1',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                   '\0', '\0', '\0', '\0', '\x10', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf5) == false);

  /* File size too big (run only when SIZE_MAX < UINT64_MAX).  */
  if (SIZE_MAX < UINT64_MAX) {
    char buf6[24] = {'M',    'S',    'G',    '\0',   '\1',   '\0',   '\0',   '\0',
                     '\0',   '\0',   '\0',   '\0',   '\0',   '\0',   '\0',   '\0',
                     '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff'};
    REQUIRE(__nl_types_check_cat_header(buf6) == false);
  }

  /* Too many sets for the stated size of the file.  */
  char buf7[24] = {'M',  'S',  'G',  '\0', '\1',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                   '\1', '\0', '\0', '\0', '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf7) == false);

  /* Right number of sets for file. */
  char buf8[24] = {'M',    'S',  'G',  '\0', '\1',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                   '\x0a', '\0', '\0', '\0', '\xff', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf8) == true);
}
