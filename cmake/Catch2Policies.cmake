# Catch2 v2.13.7 supports our policy baseline but only declares a minimum.
# Preserve that minimum and declare the policy range when fetching its sources.
set(_catch2_cmake "${SOURCE_DIR}/CMakeLists.txt")
file(READ "${_catch2_cmake}" _catch2_contents)
set(_catch2_original "cmake_minimum_required(VERSION 3.5)")
set(_catch2_updated "cmake_minimum_required(VERSION 3.5...3.15)")
string(FIND "${_catch2_contents}" "${_catch2_updated}" _catch2_patched)
if(_catch2_patched EQUAL -1)
  string(FIND "${_catch2_contents}" "${_catch2_original}" _catch2_found)
  if(_catch2_found EQUAL -1)
    message(FATAL_ERROR "Unexpected Catch2 minimum CMake version declaration")
  endif()
  string(REPLACE "${_catch2_original}" "${_catch2_updated}"
                 _catch2_contents "${_catch2_contents}")
  file(WRITE "${_catch2_cmake}" "${_catch2_contents}")
endif()
