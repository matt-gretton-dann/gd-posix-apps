# \file   CMakeLists.txt
# \author Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

add_library(libawk)
target_include_directories(libawk PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(libawk PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(libawk PUBLIC gdsup util)
set_property(TARGET libawk PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET libawk PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_definitions(libawk PRIVATE _CRT_SECURE_NO_WARNINGS)
if (ENABLE_NON_POSIX_EXTENSIONS)
  target_compile_definitions(libawk PRIVATE ENABLE_EXTENSIONS=1)
endif ()
set_warnings(libawk)

target_sources(libawk PRIVATE execute.cc instruction.cc lexer.cc parsed-program.cc parser.cc reader.cc token.cc)
target_sources(libawk PUBLIC awk.hh)
add_dependencies(libawk awk_messages_header)
target_add_clang_format(libawk)

if (RUN_UNIT_TESTS)
  add_executable(test-libawk)
  if (ENABLE_NON_POSIX_EXTENSIONS)
    target_compile_definitions(test-libawk PRIVATE ENABLE_EXTENSIONS=1)
  endif ()
  target_link_libraries(test-libawk PUBLIC libawk Catch2::Catch2)
  set_property(TARGET test-libawk PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-libawk PROPERTY CXX_STANDARD_REQUIRED ON)
  target_include_directories(test-libawk PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_warnings(test-libawk)
  target_sources(test-libawk PRIVATE
          test-libawk-main.cc
          test-lexer.cc
          test-reader.cc)
  target_add_clang_format(test-libawk)
  catch_discover_tests(test-libawk PROPERTIES FIXTURES_REQUIRED testutil_setup EXTRA_ARGS " -s")
endif ()

add_utility(NAME awk NO_INT_TEST CAT_ID awk SOURCES awk.cc)
target_link_libraries(awk PUBLIC libawk)

if (NOT WIN32)
  add_gdpat_test(NAME awk
          XFAILS
          "awk/onetrueawk-awk"
          SKIPS
          "awk/onetrueawk-awk/basic/p.14" "awk/onetrueawk-awk/basic/p.15" "awk/onetrueawk-awk/basic/p.16"
          "awk/onetrueawk-awk/basic/p.17" "awk/onetrueawk-awk/basic/p.18" "awk/onetrueawk-awk/basic/p.19"
          "awk/onetrueawk-awk/basic/p.37" "awk/onetrueawk-awk/basic/p.41" "awk/onetrueawk-awk/basic/p.47"
          "awk/onetrueawk-awk/basic/p.49" "awk/onetrueawk-awk/general/t.addops"
          "awk/onetrueawk-awk/general/t.aeiouy" "awk/onetrueawk-awk/general/t.comment"
          "awk/onetrueawk-awk/general/t.delete0" "awk/onetrueawk-awk/general/t.delete1"
          "awk/onetrueawk-awk/general/t.delete3" "awk/onetrueawk-awk/general/t.do"
          "awk/onetrueawk-awk/general/t.f0" "awk/onetrueawk-awk/general/t.printf2"
          "awk/onetrueawk-awk/general/t.re7" "awk/onetrueawk-awk/general/t.redir1"
          "awk/onetrueawk-awk/general/t.split9" "awk/onetrueawk-awk/general/t.split9a"
          "awk/onetrueawk-awk/advanced/T.beebe" "awk/onetrueawk-awk/advanced/T.latin1"
          "awk/onetrueawk-awk/advanced/T.nextfile" "awk/onetrueawk-awk/advanced/T.re"
          "awk/onetrueawk-awk/advanced/T.sub" "awk/onetrueawk-awk/advanced/T.expr"
          )
endif ()
