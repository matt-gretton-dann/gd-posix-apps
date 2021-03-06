# \file   cmake/Warnings.cmake
# \brief  Add -Wall -Werror (or equivalent) on a target
# \author Copyright 2020, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

# Enable all warnings and treat them as errors on the given target.
# Is in theory cross-platform.
function(set_warnings TARGET)

if(MSVC)
  # We enable all warnings at level 4, but disable the following:
  #  4201 - Anonymous unions - this is legal C11.
  target_compile_options(${TARGET} PRIVATE /W4 /WX /wd4201)
else()
  target_compile_options(${TARGET} PRIVATE -Wall -Wextra -Wpedantic -Werror)
  if (APPLE)
  target_compile_options(${TARGET} PRIVATE -Wno-gnu-zero-variadic-macro-arguments)
  endif()
endif()

endfunction()
