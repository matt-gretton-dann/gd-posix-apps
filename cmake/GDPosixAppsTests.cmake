# \file   cmake/GDPosixAppsTests.cmake
# \brief  Import the external testsuite if available
# \author Copyright 2021, Matthew Gretton-Dann
#         SPDX-License-Identifier: Apache-2.0

if(NOT BUILD_TESTING)
  message(STATUS "Not using gd-posix-apps-tests as testing is disabled")
  set(USE_GD_POSIX_APPS_TESTS OFF)
elseif(NOT ALLOW_THIRD_PARTY_DOWNLOADS)
  message(STATUS "Not using gd-posix-apps-tests as Third-party downloads disabled")
  set(USE_GD_POSIX_APPS_TESTS OFF)
else()
  find_package(Python 3.6 COMPONENTS Interpreter)
  if(NOT Python_FOUND)
    message(WARNING
      "Unable to find appropriate Python 3 module (require 3.6+) - GD Posix Apps Tests disabled.")
    set(USE_GD_POSIX_APPS_TESTS OFF)
  else()
    message(STATUS "Fetching gd-posix-apps-tests")
    FetchContent_Declare(gdpat
                        GIT_REPOSITORY https://github.com/matt-gretton-dann/gd-posix-apps-tests
                        GIT_TAG main)
    FetchContent_MakeAvailable(gdpat)
    set(USE_GD_POSIX_APPS_TESTS ON)
  endif()
endif()


function(add_gdpat_test UTILITY)
  if(NOT USE_GD_POSIX_APPS_TESTS OR NOT BUILD_TESTING)
    message(STATUS "Not using gd-posix-app-tests for ${UTILITY}")
    return()
  endif()

  execute_process(
    COMMAND "${Python_EXECUTABLE}" "${gdpat_SOURCE_DIR}/run-tests.py" "--list-tests" "${UTILITY}"
    OUTPUT_VARIABLE _agt_TESTS)
  # Newline in next command is deliberate do not remove
  string(REPLACE "
" ";" _agt_TESTS "${_agt_TESTS}")
  foreach(_agt_TEST IN LISTS _agt_TESTS)
    if (_agt_TEST)
      add_test(
        NAME "${_agt_TEST}"
        COMMAND "${Python_EXECUTABLE}" "${gdpat_SOURCE_DIR}/run-tests.py"
                "--utility" "$<TARGET_FILE:${UTILITY}>"
                "${_agt_TEST}")
    endif()
  endforeach()
endfunction()
