# \file   src/CMakeLists.txt
# \author Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

# Add a utility executable along with integration tests.
# Parameters:
#    NAME <utiltiy name>
#    SOURCES <sources>, if not specified assume name.c
#    NO_INT_TEST - if present no integration tests
#    UNIT_TESTS <sources> sources for unit tests.
#    CAT_ID <catalogue ID>.
function(add_utility)
    # Command line option parsing
    set(_au_options NO_INT_TEST)
    set(_au_single_options NAME CAT_ID)
    set(_au_multi_options SOURCES UNIT_TESTS)
    cmake_parse_arguments(_au
                          "${_au_options}"
                          "${_au_single_options}"
                          "${_au_multi_options}"
                          ${ARGN})
    if(NOT DEFINED _au_NAME)
        message(FATAL_ERROR "Must specify NAME to add_utility")
    endif()
    if(NOT DEFINED _au_SOURCES)
        set(_au_SOURCES "${_au_NAME}.c")
    endif()

    # Add the executable target
    add_executable("${_au_NAME}" "${_au_SOURCES}")
    target_link_libraries("${_au_NAME}" PUBLIC gdsup)
    target_include_directories("${_au_NAME}" PRIVATE "${CMAKE_SOURCE_DIR}/src/include")
    target_add_clang_format("${_au_NAME}")
    set_warnings("${_au_NAME}")

    if(DEFINED _au_CAT_ID)
      # Done after the call to target_add_clang_format so that we don't try to format
      # message-data.hh
      add_custom_command(
        OUTPUT message-data.hh
        COMMAND "${Python_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
                --cat-id "${_au_CAT_ID}" --header "${CMAKE_CURRENT_BINARY_DIR}/message-data.hh"
                "${_au_CAT_ID}.messages.json"
        MAIN_DEPENDENCY "${_au_CAT_ID}.messages.json"
        DEPENDS "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        COMMENT "Generating i18n header for ${_au_CAT_ID}")
      target_include_directories(gencat PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
      target_sources(gencat PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/message-data.hh")

      add_custom_command(
        OUTPUT "${_au_CAT_ID}.msg.stamp"
        COMMAND "${Python_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
                --msg "${CMAKE_CURRENT_BINARY_DIR}" --cat-id "${_au_CAT_ID}"
                --gencat "$<TARGET_FILE:gencat>"
                "${_au_CAT_ID}.messages.json"
        MAIN_DEPENDENCY "${_au_CAT_ID}.messages.json"
        DEPENDS "gencat" "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        COMMENT "Generating compiled i18n messages for ${_au_CAT_ID}"
      )
      add_custom_target("${_au_CAT_ID}_messages" ALL DEPENDS "${_au_CAT_ID}.msg.stamp")
    endif()

    # Add the integration test
    if(RUN_INTEGRATION_TESTS AND NOT NO_INT_TEST)
        file(TO_NATIVE_PATH "${CMAKE_SOURCE_DIR}/python-lib" _au_python_lib)
        file(TO_NATIVE_PATH "${CMAKE_BINARY_DIR}/pycache" _au_python_cache)
        set(_au_INT_TEST "int-test-${_au_NAME}")
        if($ENV{PYTHONPATH})
            if(CMAKE_HOST_UNIX)
                set(_au_path_sep ":")
            elseif(CMAKE_HOST_WIN32)
                set(_au_path_sep ";")
            else()
                message(FATAL_ERROR "Unable to determine path separator - host is not UNIX or WIN32")
            endif()
            set(_au_python_lib "${_au_python_lib}${_au_path_sep}$ENV{PYTHONPATH}")
        endif()

        file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${_au_INT_TEST}.py" _au_TEST_PATH)
        file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/test-data" _au_TEST_DATA)
        file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" _au_TEST_OUTPUT)

        add_test(NAME "${_au_INT_TEST}"
                 COMMAND "${Python_EXECUTABLE}" "${_au_TEST_PATH}" "$<TARGET_FILE:${_au_NAME}>"
                         "--input-dir" "${_au_TEST_DATA}" "--output-dir" "${_au_TEST_OUTPUT}"
                         "--nls-path" "${_au_TEST_OUTPUT}")
        set_tests_properties("${_au_INT_TEST}"
                             PROPERTIES ENVIRONMENT
                                        "PYTHONPATH=${_au_python_lib};PYTHONPYCACHEPREFIX=${_au_python_cache}")
    endif()
endfunction()

add_subdirectory(gencat)
add_subdirectory(true-false)
