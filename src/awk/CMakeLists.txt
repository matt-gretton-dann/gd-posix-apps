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
  catch_discover_tests(test-libawk PROPERTIES FIXTURES_REQUIRED testutil_setup EXTRA_ARGS "-s")
endif ()

add_utility(NAME awk CAT_ID awk SOURCES awk.cc)
target_link_libraries(awk PUBLIC libawk)

if (NOT WIN32)
  add_gdpat_test(NAME awk
          XFAILS
          "awk/onetrueawk-awk/basic/p.29"
          "awk/onetrueawk-awk/basic/p.32"
          "awk/onetrueawk-awk/basic/p.33"
          "awk/onetrueawk-awk/basic/p.34"
          "awk/onetrueawk-awk/basic/p.35"
          "awk/onetrueawk-awk/basic/p.36"
          "awk/onetrueawk-awk/basic/p.41"
          "awk/onetrueawk-awk/basic/p.43"
          "awk/onetrueawk-awk/basic/p.44"
          "awk/onetrueawk-awk/basic/p.47"
          "awk/onetrueawk-awk/basic/p.48"
          "awk/onetrueawk-awk/basic/p.48a"
          "awk/onetrueawk-awk/basic/p.48b"
          "awk/onetrueawk-awk/basic/p.49"
          "awk/onetrueawk-awk/basic/p.50"
          "awk/onetrueawk-awk/basic/p.51"
          "awk/onetrueawk-awk/basic/p.52"
          "awk/onetrueawk-awk/basic/p.table"

          "awk/onetrueawk-awk/general/t.1"
          "awk/onetrueawk-awk/general/t.1.x"
          "awk/onetrueawk-awk/general/t.2"
          "awk/onetrueawk-awk/general/t.2.x"
          "awk/onetrueawk-awk/general/t.3.x"
          "awk/onetrueawk-awk/general/t.5.x"
          "awk/onetrueawk-awk/general/t.6"
          "awk/onetrueawk-awk/general/t.6.x"
          "awk/onetrueawk-awk/general/t.6a"
          "awk/onetrueawk-awk/general/t.6b"
          "awk/onetrueawk-awk/general/t.8.x"
          "awk/onetrueawk-awk/general/t.8.y"
          "awk/onetrueawk-awk/general/t.NF"
          "awk/onetrueawk-awk/general/t.a"
          "awk/onetrueawk-awk/general/t.addops"
          "awk/onetrueawk-awk/general/t.arith"
          "awk/onetrueawk-awk/general/t.array"
          "awk/onetrueawk-awk/general/t.array1"
          "awk/onetrueawk-awk/general/t.array2"
          "awk/onetrueawk-awk/general/t.assert"
          "awk/onetrueawk-awk/general/t.avg"
          "awk/onetrueawk-awk/general/t.b.x"
          "awk/onetrueawk-awk/general/t.beginexit"
          "awk/onetrueawk-awk/general/t.beginnext"
          "awk/onetrueawk-awk/general/t.break"
          "awk/onetrueawk-awk/general/t.break1"
          "awk/onetrueawk-awk/general/t.break2"
          "awk/onetrueawk-awk/general/t.break3"
          "awk/onetrueawk-awk/general/t.bug1"
          "awk/onetrueawk-awk/general/t.builtins"
          "awk/onetrueawk-awk/general/t.cat"
          "awk/onetrueawk-awk/general/t.cat2"
          "awk/onetrueawk-awk/general/t.cmp"
          "awk/onetrueawk-awk/general/t.coerce2"
          "awk/onetrueawk-awk/general/t.cond"
          "awk/onetrueawk-awk/general/t.contin"
          "awk/onetrueawk-awk/general/t.crlf"
          "awk/onetrueawk-awk/general/t.cum"
          "awk/onetrueawk-awk/general/t.d.x"
          "awk/onetrueawk-awk/general/t.delete0"
          "awk/onetrueawk-awk/general/t.delete1"
          "awk/onetrueawk-awk/general/t.delete2"
          "awk/onetrueawk-awk/general/t.delete3"
          "awk/onetrueawk-awk/general/t.do"
          "awk/onetrueawk-awk/general/t.e"
          "awk/onetrueawk-awk/general/t.exit"
          "awk/onetrueawk-awk/general/t.exit1"
          "awk/onetrueawk-awk/general/t.f"
          "awk/onetrueawk-awk/general/t.f.x"
          "awk/onetrueawk-awk/general/t.f1"
          "awk/onetrueawk-awk/general/t.f2"
          "awk/onetrueawk-awk/general/t.f3"
          "awk/onetrueawk-awk/general/t.f2"
          "awk/onetrueawk-awk/general/t.f4"
          "awk/onetrueawk-awk/general/t.for"
          "awk/onetrueawk-awk/general/t.for1"
          "awk/onetrueawk-awk/general/t.for2"
          "awk/onetrueawk-awk/general/t.for3"
          "awk/onetrueawk-awk/general/t.format4"
          "awk/onetrueawk-awk/general/t.fun"
          "awk/onetrueawk-awk/general/t.fun0"
          "awk/onetrueawk-awk/general/t.fun1"
          "awk/onetrueawk-awk/general/t.fun2"
          "awk/onetrueawk-awk/general/t.fun3"
          "awk/onetrueawk-awk/general/t.fun4"
          "awk/onetrueawk-awk/general/t.fun5"
          "awk/onetrueawk-awk/general/t.getline1"
          "awk/onetrueawk-awk/general/t.getval"
          "awk/onetrueawk-awk/general/t.gsub"
          "awk/onetrueawk-awk/general/t.gsub1"
          "awk/onetrueawk-awk/general/t.gsub3"
          "awk/onetrueawk-awk/general/t.gsub4"
          "awk/onetrueawk-awk/general/t.i.x"
          "awk/onetrueawk-awk/general/t.if"
          "awk/onetrueawk-awk/general/t.in"
          "awk/onetrueawk-awk/general/t.in1"
          "awk/onetrueawk-awk/general/t.in2"
          "awk/onetrueawk-awk/general/t.in3"
          "awk/onetrueawk-awk/general/t.incr3"
          "awk/onetrueawk-awk/general/t.index"
          "awk/onetrueawk-awk/general/t.intest"
          "awk/onetrueawk-awk/general/t.intest2"
          "awk/onetrueawk-awk/general/t.j.x"
          "awk/onetrueawk-awk/general/t.longstr"
          "awk/onetrueawk-awk/general/t.makef"
          "awk/onetrueawk-awk/general/t.match"
          "awk/onetrueawk-awk/general/t.match1"
          "awk/onetrueawk-awk/general/t.nameval"
          "awk/onetrueawk-awk/general/t.next"
          "awk/onetrueawk-awk/general/t.not"
          "awk/onetrueawk-awk/general/t.null0"
          "awk/onetrueawk-awk/general/t.ofmt"
          "awk/onetrueawk-awk/general/t.ofs"
          "awk/onetrueawk-awk/general/t.ors"
          "awk/onetrueawk-awk/general/t.pipe"
          "awk/onetrueawk-awk/general/t.printf"
          "awk/onetrueawk-awk/general/t.randk"
          "awk/onetrueawk-awk/general/t.re2"
          "awk/onetrueawk-awk/general/t.re5"
          "awk/onetrueawk-awk/general/t.reFS"
          "awk/onetrueawk-awk/general/t.rec"
          "awk/onetrueawk-awk/general/t.redir1"
          "awk/onetrueawk-awk/general/t.reg"
          "awk/onetrueawk-awk/general/t.roff"
          "awk/onetrueawk-awk/general/t.sep"
          "awk/onetrueawk-awk/general/t.set0"
          "awk/onetrueawk-awk/general/t.set0a"
          "awk/onetrueawk-awk/general/t.set0b"
          "awk/onetrueawk-awk/general/t.set1"
          "awk/onetrueawk-awk/general/t.set2"
          "awk/onetrueawk-awk/general/t.set3"
          "awk/onetrueawk-awk/general/t.split1"
          "awk/onetrueawk-awk/general/t.split2"
          "awk/onetrueawk-awk/general/t.split2a"
          "awk/onetrueawk-awk/general/t.split3"
          "awk/onetrueawk-awk/general/t.split4"
          "awk/onetrueawk-awk/general/t.split8"
          "awk/onetrueawk-awk/general/t.split9"
          "awk/onetrueawk-awk/general/t.split9a"
          "awk/onetrueawk-awk/general/t.strcmp"
          "awk/onetrueawk-awk/general/t.strnum"
          "awk/onetrueawk-awk/general/t.sub0"
          "awk/onetrueawk-awk/general/t.sub1"
          "awk/onetrueawk-awk/general/t.sub2"
          "awk/onetrueawk-awk/general/t.sub3"
          "awk/onetrueawk-awk/general/t.substr"
          "awk/onetrueawk-awk/general/t.substr1"
          "awk/onetrueawk-awk/general/t.vf"
          "awk/onetrueawk-awk/general/t.vf1"
          "awk/onetrueawk-awk/general/t.vf2"
          "awk/onetrueawk-awk/general/t.vf3"

          "awk/onetrueawk-awk/advanced/T.-f-f"
          "awk/onetrueawk-awk/advanced/T.argv"
          "awk/onetrueawk-awk/advanced/T.arnold"
          "awk/onetrueawk-awk/advanced/T.beebe"
          "awk/onetrueawk-awk/advanced/T.builtin"
          "awk/onetrueawk-awk/advanced/T.chem"
          "awk/onetrueawk-awk/advanced/T.close"
          "awk/onetrueawk-awk/advanced/T.clv"
          "awk/onetrueawk-awk/advanced/T.csconcat"
          "awk/onetrueawk-awk/advanced/T.delete"
          "awk/onetrueawk-awk/advanced/T.errmsg"
          "awk/onetrueawk-awk/advanced/T.exprconv"
          "awk/onetrueawk-awk/advanced/T.flags"
          "awk/onetrueawk-awk/advanced/T.func"
          "awk/onetrueawk-awk/advanced/T.gawk"
          "awk/onetrueawk-awk/advanced/T.getline"
          "awk/onetrueawk-awk/advanced/T.int-expr"
          "awk/onetrueawk-awk/advanced/T.lilly"
          "awk/onetrueawk-awk/advanced/T.main"
          "awk/onetrueawk-awk/advanced/T.misc"
          "awk/onetrueawk-awk/advanced/T.overflow"
          "awk/onetrueawk-awk/advanced/T.recache"
          "awk/onetrueawk-awk/advanced/T.redir"
          "awk/onetrueawk-awk/advanced/T.split"
          "awk/onetrueawk-awk/advanced/T.system"

          SKIPS
          "awk/onetrueawk-awk/general/t.printf2"
          "awk/onetrueawk-awk/advanced/T.expr"
          "awk/onetrueawk-awk/advanced/T.latin1"
          "awk/onetrueawk-awk/advanced/T.nextfile"
          "awk/onetrueawk-awk/advanced/T.re"
          "awk/onetrueawk-awk/advanced/T.sub"
          )
endif ()
