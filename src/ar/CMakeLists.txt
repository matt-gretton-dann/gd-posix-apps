# \file   CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


add_library(libar)
target_include_directories(libar PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(libar PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(libar PUBLIC gdsup util)
set_property(TARGET libar PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET libar PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_definitions(libar PRIVATE _CRT_SECURE_NO_WARNINGS)
if(ENABLE_NON_POSIX_EXTENSIONS)
  target_compile_definitions(libar PRIVATE ENABLE_EXTENSIONS=1)
endif()
set_warnings(libar)

target_sources(libar PRIVATE ar-member.cc)
add_dependencies(libar ar_messages_header)
target_add_clang_format(libar)

if(RUN_UNIT_TESTS)
  add_executable(test-libar)
  if(ENABLE_NON_POSIX_EXTENSIONS)
    target_compile_definitions(test-libar PRIVATE ENABLE_EXTENSIONS=1)
  endif()
  target_link_libraries(test-libar PUBLIC libar Catch2::Catch2)
  set_property(TARGET test-libar PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-libar PROPERTY CXX_STANDARD_REQUIRED ON)
  target_include_directories(test-libar PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_warnings(test-libar)
  target_sources(test-libar PRIVATE
                 test-libar-main.cc
                 test-member-header.cc
                 )
  target_add_clang_format(test-libar)
  catch_discover_tests(test-libar PROPERTIES FIXTURES_REQUIRED testutil_setup EXTRA_ARGS "-s")
endif()

add_utility(NAME ar NO_INT_TEST CAT_ID ar SOURCES ar.cc)
target_link_libraries(ar PUBLIC libar)
