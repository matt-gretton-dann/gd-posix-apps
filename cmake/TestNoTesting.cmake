# Verify that disabling testing neither creates test targets nor requires Catch.
# SPDX-License-Identifier: Apache-2.0
if(NOT DEFINED TEST_SOURCE_DIR OR NOT DEFINED TEST_BINARY_DIR OR NOT DEFINED TEST_GENERATOR)
  message(FATAL_ERROR "Missing nested configure parameters")
endif()
execute_process(
  COMMAND "${CMAKE_COMMAND}" -S "${TEST_SOURCE_DIR}" -B "${TEST_BINARY_DIR}"
          -G "${TEST_GENERATOR}" -DBUILD_TESTING=OFF -DALLOW_THIRD_PARTY_DOWNLOADS=OFF
          "-DCMAKE_C_COMPILER:FILEPATH=${TEST_C_COMPILER}"
          "-DCMAKE_CXX_COMPILER:FILEPATH=${TEST_CXX_COMPILER}"
  RESULT_VARIABLE _result OUTPUT_VARIABLE _output ERROR_VARIABLE _error TIMEOUT 90)
if(NOT _result STREQUAL "0")
  message(FATAL_ERROR "BUILD_TESTING=OFF configure failed (${_result}):\n${_output}\n${_error}")
endif()
file(READ "${TEST_BINARY_DIR}/CMakeFiles/TargetDirectories.txt" _targets)
foreach(_target test-gdsup test-util test-libbc)
  if(_targets MATCHES "/${_target}\\.dir")
    message(FATAL_ERROR "Disabled testing still creates ${_target}")
  endif()
endforeach()
