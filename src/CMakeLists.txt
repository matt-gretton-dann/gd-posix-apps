# \file   src/CMakeLists.txt
# \author Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

# Add a utility executable along with integration tests.
# Parameters:
#    NAME <utiltiy name>
#    SOURCES <sources>, if not specified assume name.c
#    NO_INT_TEST - if present no integration tests
#    UNIT_TESTS <sources> sources for unit tests. (TBD)
function(add_utility)
    # Command line option parsing
    set(_au_options NO_INT_TEST)
    set(_au_single_options NAME)
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

    # Add the integration test
    if(RUN_INTEGRATION_TESTS AND NOT NO_INT_TEST)
        file(TO_NATIVE_PATH "${CMAKE_SOURCE_DIR}/python-lib" _au_python_lib)
        file(TO_NATIVE_PATH "${CMAKE_BINARY_DIR}/pycache" _au_python_cache)
        set(_au_INT_TEST "int-test-${_au_NAME}")
        if($ENV{PYTHONPATH})
            if(CMAKE_HOST_UNIX)
                set(_au_path_sep ":")
            elseif(CMAKE_HOST_WIN32)
                set(path_sep ";")
            else()
                message(FATAL_ERROR "Unable to determine path separator - host is not UNIX or WIN32")
            endif()
            set(_au_python_lib "${_au_python_lib}${_au_path_sep}$ENV{PYTHONPATH}")
        endif()

        add_test(NAME "${_au_INT_TEST}"
                 COMMAND "${Python_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/${_au_INT_TEST}.py"
                         "$<TARGET_FILE:${_au_NAME}>")
        set_tests_properties("${_au_INT_TEST}" 
                             PROPERTIES ENVIRONMENT 
                                        "PYTHONPATH=${_au_python_lib};PYTHONPYCACHEPREFIX=${_au_python_cache}")
    endif()
endfunction()

add_subdirectory(true-false)