target_sources(util PRIVATE program-name.cc)

if(RUN_UNIT_TESTS)
  target_sources(test-util PRIVATE
    test-program-name.cc
    test-util-main.cc)
endif()