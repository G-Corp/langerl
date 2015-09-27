cmake_minimum_required(VERSION 3.0)
project(NODECOMPLEX)

file(GLOB SOURCES src/*)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_HOME_DIRECTORY}/priv)
add_executable(node.complex
  ${SOURCES}
  ${LANGERL_COMMON_SOURCES}
  )
target_link_libraries(node.complex ${LANGERL_COMMON_LIBRARIES})
