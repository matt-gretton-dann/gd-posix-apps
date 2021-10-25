add_library(libcpp)
target_include_directories(libcpp PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")
target_include_directories(libcpp PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/..")
target_include_directories(libcpp PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
target_link_libraries(libcpp PUBLIC gdsup util)
set_property(TARGET libcpp PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
set_property(TARGET libcpp PROPERTY CXX_STANDARD_REQUIRED ON)
target_compile_definitions(libcpp PRIVATE _CRT_SECURE_NO_WARNINGS)
if(ENABLE_NON_POSIX_EXTENSIONS)
  target_compile_definitions(libcpp PRIVATE ENABLE_EXTENSIONS=1)
endif()
set_warnings(libcpp)

target_sources(libcpp PRIVATE
    error.cc
    file-store.cc
    identifier-manager.cc
    token.cc
    trigraph-parser.cc)
target_sources(libcpp PRIVATE
    include/error.hh
    include/file-store.hh
    include/identifier-manager.hh
    include/location.hh
    include/preprocessor-tokenizer.hh
    include/simple-tokenizers.hh
    include/token.hh
    include/tokenizer.hh)
#add_dependencies("libcpp" "cc_messages_header")
target_add_clang_format(libcpp)

if(RUN_UNIT_TESTS)
  add_executable(test-libcpp)
  if(ENABLE_NON_POSIX_EXTENSIONS)
    target_compile_definitions(test-libcpp PRIVATE ENABLE_EXTENSIONS=1)
  endif()
  target_link_libraries(test-libcpp PUBLIC libcpp Catch2::Catch2)
  set_property(TARGET test-libcpp PROPERTY CXX_STANDARD "${CMAKE_CXX_STANDARD}")
  set_property(TARGET test-libcpp PROPERTY CXX_STANDARD_REQUIRED ON)
  target_include_directories(test-libcpp PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
  set_warnings(test-libcpp)
  target_sources(test-libcpp PRIVATE
                 test-file-store.cc
                 test-identifier-manager.cc
                 test-location.cc
                 test-libcpp-main.cc
                 test-newline-chewer.cc
                 test-preprocessor-tokenizer.cc
                 test-trigraph-parser.cc)
  target_add_clang_format(test-libcpp)
  catch_discover_tests(test-libcpp PROPERTIES FIXTURES_REQUIRED testutil_setup EXTRA_ARGS "-s")
endif()
