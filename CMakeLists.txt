cmake_minimum_required(VERSION 3.0)
project(LANGERL)

include("${LANGERL_SOURCE_DIR}/configuration.txt")

SET(MODS ${CMAKE_SOURCE_DIR}/CMake/erlang)
SET(CMAKE_MODULE_PATH ${MODS} ${CMAKE_MODULE_PATH})

FIND_PACKAGE(Erlang REQUIRED)
FIND_PACKAGE(Threads REQUIRED)

include_directories(
  ${CMAKE_HOME_DIRECTORY}/include
  ${ERLANG_EI_INCLUDE_PATH}
  ${ERLANG_ERTS_INCLUDE_PATH}
  )
link_directories(
  ${ERLANG_EI_LIBRARY_PATH}
  )
set(LANGERL_COMMON_LIBRARIES erl_interface ei nsl ${CMAKE_THREAD_LIBS_INIT})
file(GLOB LANGERL_COMMON_SOURCES ${CMAKE_HOME_DIRECTORY}/src/*)

add_subdirectory (ruby)
