# Explicit control of project-managed CMake IPO flags.
# SPDX-License-Identifier: Apache-2.0
include(CheckIPOSupported)

set(IPO_MODE AUTO CACHE STRING "Automatic IPO policy: AUTO, OFF or REQUIRED")
set_property(CACHE IPO_MODE PROPERTY STRINGS AUTO OFF REQUIRED)

function(gd_check_ipo)
  # A failed reconfiguration must not leave a previous successful selection
  # available as evidence for the newly requested mode.
  file(REMOVE "${CMAKE_BINARY_DIR}/gd-ipo-policy.json")
  if(NOT IPO_MODE STREQUAL "AUTO" AND NOT IPO_MODE STREQUAL "OFF"
     AND NOT IPO_MODE STREQUAL "REQUIRED")
    message(FATAL_ERROR "Unsupported IPO_MODE: ${IPO_MODE}; use AUTO, OFF or REQUIRED")
  endif()
  set(_enabled OFF)
  if(IPO_MODE STREQUAL "OFF")
    message(STATUS "Automatic IPO disabled by IPO_MODE=OFF")
  elseif(IPO_MODE STREQUAL "AUTO" AND CMAKE_BUILD_TYPE STREQUAL "Debug")
    message(STATUS "Automatic IPO disabled for Debug builds")
  else()
    check_ipo_supported(RESULT _enabled OUTPUT _warnings LANGUAGES C CXX)
    if(_enabled)
      message(STATUS "Inter-procedural optimizations are enabled.")
    elseif(IPO_MODE STREQUAL "REQUIRED")
      message(FATAL_ERROR "IPO_MODE=REQUIRED but IPO is unsupported: ${_warnings}")
    else()
      message(STATUS "Inter-procedural optimizations are not supported for this build.")
    endif()
  endif()
  set(ENABLE_IPO "${_enabled}" PARENT_SCOPE)
  if(_enabled)
    set(_json_enabled true)
  else()
    set(_json_enabled false)
  endif()
  file(WRITE "${CMAKE_BINARY_DIR}/gd-ipo-policy.json"
       "{\"schema\":1,\"mode\":\"${IPO_MODE}\",\"automatic_ipo\":${_json_enabled}}\n")
endfunction()

function(gd_apply_ipo target)
  if(ENABLE_IPO)
    set_property(TARGET "${target}" PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
  elseif(IPO_MODE STREQUAL "OFF")
    # An explicit opt-out also overrides an inherited CMake IPO default on
    # targets managed here. Caller-supplied compiler/link flags stay explicit.
    set_property(TARGET "${target}" PROPERTY INTERPROCEDURAL_OPTIMIZATION FALSE)
    # Per-configuration defaults are copied onto a new target and take
    # precedence over the general property, including custom configurations.
    foreach(_gd_config DEBUG RELEASE RELWITHDEBINFO MINSIZEREL
                       ${CMAKE_CONFIGURATION_TYPES} ${CMAKE_BUILD_TYPE})
      string(TOUPPER "${_gd_config}" _gd_config_upper)
      set_property(TARGET "${target}" PROPERTY
                   "INTERPROCEDURAL_OPTIMIZATION_${_gd_config_upper}" FALSE)
    endforeach()
  endif()
endfunction()
