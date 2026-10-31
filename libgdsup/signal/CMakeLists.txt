# \file   libgdsup/signal/CMakeLists.txt
# \brief  CMakeLists.txt for signal.h implementation
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if(WIN32)
add_gdsup_source(
  SOURCES sigaction-win32.c
          sigaddset-win32.c
          sigdelset-win32.c
          sigemptyset-win32.c
          sigfillset-win32.c)

endif()

if(RUN_UNIT_TESTS)
  add_executable(test-signal-first-action test-first-action.cc)
  target_link_libraries(test-signal-first-action PRIVATE gdsup)
  set_property(TARGET test-signal-first-action PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-signal-first-action PROPERTY CXX_STANDARD_REQUIRED ON)
  set_warnings(test-signal-first-action)
  target_add_clang_format(test-signal-first-action)
  add_test(NAME gdsup/signal-first-action COMMAND "$<TARGET_FILE:test-signal-first-action>")
  set_tests_properties(gdsup/signal-first-action PROPERTIES TIMEOUT 10 SKIP_RETURN_CODE 77)
endif()
