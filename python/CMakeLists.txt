cmake_minimum_required(VERSION 3.0)
project(NODERB)

FIND_PACKAGE(PythonLibs REQUIRED)

include_directories(
  ${PYTHON_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  )

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_HOME_DIRECTORY}/priv)
add_executable(node.python
  ${SOURCES}
  ${LANGERL_COMMON_SOURCES}
  )
target_link_libraries(node.python ${PYTHON_LIBRARIES})
target_link_libraries(node.python ${LANGERL_COMMON_LIBRARIES})
