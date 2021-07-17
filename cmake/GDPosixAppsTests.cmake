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


function(add_gdpat_test)
  set(_agt_options "")
  set(_agt_single_options NAME)
  set(_agt_multi_options XFAILS SKIPS)
  cmake_parse_arguments(_agt
                        "${_agt_options}"
                        "${_agt_single_options}"
                        "${_agt_multi_options}"
                        ${ARGN})
  if(NOT DEFINED _agt_NAME)
    message(FATAL_ERROR "Must specify NAME to add_gdpat_test")
  endif()

  if(NOT USE_GD_POSIX_APPS_TESTS OR NOT BUILD_TESTING)
    message(STATUS "Not using gd-posix-app-tests for ${UTILITY}")
    return()
  endif()


  execute_process(
    COMMAND "${Python_EXECUTABLE}" "${gdpat_SOURCE_DIR}/run-tests.py" "--list-tests" "${_agt_NAME}"
    OUTPUT_VARIABLE _agt_TESTS)
  # Newline in next command is deliberate do not remove
  string(REPLACE "
" ";" _agt_TESTS "${_agt_TESTS}")

  foreach(_agt_TEST IN LISTS _agt_TESTS)
    if(_agt_TEST)
      if(DEFINED _agt_SKIPS)
        set(_agt_SKIP_TEST OFF)
        foreach(_agt_SKIP IN LISTS _agt_SKIPS)
          if("${_agt_TEST}" MATCHES "^${_agt_SKIP}/" OR "${_agt_TEST}" STREQUAL "${_agt_SKIP}")
            message(STATUS "Skipping ${_agt_TEST} as it matches skip: ${_agt_SKIP}")
            set(_agt_SKIP_TEST ON)
            break()
          endif()
        endforeach()
      endif()
      if(_agt_SKIP_TEST)
        add_test(
          NAME "${_agt_TEST}"
          COMMAND "${CMAKE_COMMAND}" -E echo "SKIP: ${_agt_TEST}")
        set_property(TEST ${_agt_TEST} PROPERTY SKIP_REGULAR_EXPRESSION "SKIP:")
      else()
        set(_agt_XFAIL_OPTS)
        if(DEFINED _agt_XFAILS)
          foreach(_agt_XFAIL IN LISTS _agt_XFAILS)
            if("${_agt_TEST}" MATCHES "^${_agt_XFAIL}/" OR "${_agt_TEST}" STREQUAL "${_agt_XFAIL}")
              set(_agt_XFAIL_OPTS "--expected-fail=${_agt_TEST}")
            endif()
          endforeach()
        endif()
        add_test(
          NAME "${_agt_TEST}"
          COMMAND "${Python_EXECUTABLE}" "${gdpat_SOURCE_DIR}/run-tests.py"
                  "--utility" "$<TARGET_FILE:${_agt_NAME}>"
                  "${_agt_TEST}" ${_agt_XFAIL_OPTS})
      endif()
    endif()
  endforeach()
endfunction()
