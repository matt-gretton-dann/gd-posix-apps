target_sources(util PRIVATE
  input-file/input-file-constructor.cc
  input-file/input-file-getc.cc
  input-file/input-file-queries.cc
  input-file/input-file-report-error.cc
  program-name.cc
)

add_custom_command(
  OUTPUT "util-messages.hh"
  COMMAND "${Python_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
          --cat-id util --header "${CMAKE_CURRENT_BINARY_DIR}/util-messages.hh"
          "${CMAKE_CURRENT_SOURCE_DIR}/util.messages.json"
  MAIN_DEPENDENCY "util.messages.json"
  DEPENDS "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
  COMMENT "Generating i18n header for util")
add_custom_target("util_messages_header" ALL DEPENDS "util-messages.hh")
target_include_directories(util PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
add_dependencies(util util_messages_header)

add_custom_command(
  OUTPUT "util.msg.stamp"
  COMMAND "${Python_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
          --msg "${CMAKE_CURRENT_BINARY_DIR}" --cat-id "util"
          --gencat "$<TARGET_FILE:gencat>"
          "${CMAKE_CURRENT_SOURCE_DIR}/util.messages.json"
  MAIN_DEPENDENCY "util.messages.json"
  DEPENDS "gencat" "${CMAKE_SOURCE_DIR}/python-lib/generate-messages.py"
  COMMENT "Generating compiled i18n messages for util"
)
add_custom_target("util_messages" ALL DEPENDS "util.msg.stamp")

if(RUN_UNIT_TESTS)
  target_sources(test-util PRIVATE
    test-for-each-file.cc
    test-program-name.cc
    test-util-main.cc)
endif()
