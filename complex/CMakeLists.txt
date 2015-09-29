cmake_minimum_required(VERSION 3.0)
project(NODECOMPLEX)

include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  )

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_HOME_DIRECTORY}/priv)
add_executable(node.complex
  ${SOURCES}
  ${LANGERL_COMMON_SOURCES}
  )
target_link_libraries(node.complex ${LANGERL_COMMON_LIBRARIES})
