/** \file   libgdsup/nl_types/test-catclose.cc
 *  \brief  Unit tests for catclose()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include "nl_types/nl_types.h"

TEST_CASE("__nl_types_check_cat_header", "[nl_types][nl_types_check_cat_header]")
{
  /* Check a correct buffer.  */
  constexpr size_t buf_size = 24;
  std::array<char, buf_size> buf = {'M',    'S',  'G',  '\0', '\1', '\0', '\0', '\0',
                                    '\0',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                                    '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf.data()) == true);

  /* Slight change in buffer.  */
  std::array<char, buf_size> buf2 = {'M',    's',  'G',  '\0', '\1', '\0', '\0', '\0',
                                     '\0',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                                     '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf2.data()) == false);

  /* Wrong version.  */
  std::array<char, buf_size> buf3 = {'M',    'S',  'G',  '\0', '\2', '\0', '\0', '\0',
                                     '\0',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                                     '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf3.data()) == false);

  /* Bad padding fields.  */
  std::array<char, buf_size> buf4 = {'M',    'S',  'G',  '\0', '\1', '\0', '\0', 'B',
                                     'A',    'D',  '\0', '\0', '\0', '\0', '\0', '\0',
                                     '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf4.data()) == false);

  /* File size too small.  */
  std::array<char, buf_size> buf5 = {'M',    'S',  'G',  '\0', '\1', '\0', '\0', '\0',
                                     '\0',   '\0', '\0', '\0', '\0', '\0', '\0', '\0',
                                     '\x10', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf5.data()) == false);

  /* File size too big (run only when SIZE_MAX < UINT64_MAX).  */
  if constexpr (SIZE_MAX < UINT64_MAX) {
    std::array<char, buf_size> buf6 = {
      'M',  'S',  'G',  '\0', '\1',   '\0',   '\0',   '\0',   '\0',   '\0',   '\0',   '\0',
      '\0', '\0', '\0', '\0', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff', '\xff'};
    REQUIRE(__nl_types_check_cat_header(buf6.data()) == false);
  }

  /* Too many sets for the stated size of the file.  */
  std::array<char, buf_size> buf7 = {'M',    'S',  'G',  '\0', '\1', '\0', '\0', '\0',
                                     '\0',   '\0', '\0', '\0', '\1', '\0', '\0', '\0',
                                     '\x18', '\0', '\0', '\0', '\0', '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf7.data()) == false);

  /* Right number of sets for file. */
  std::array<char, buf_size> buf8 = {'M',    'S',  'G',  '\0', '\1',   '\0', '\0', '\0',
                                     '\0',   '\0', '\0', '\0', '\x0a', '\0', '\0', '\0',
                                     '\xff', '\0', '\0', '\0', '\0',   '\0', '\0', '\0'};
  REQUIRE(__nl_types_check_cat_header(buf8.data()) == true);
}
