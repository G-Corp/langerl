cmake_minimum_required(VERSION 3.0)
project(LANGERL)

include("${LANGERL_SOURCE_DIR}/configuration.txt")

SET(MODS ${CMAKE_SOURCE_DIR}/CMake/erlang)
SET(CMAKE_MODULE_PATH ${MODS} ${CMAKE_MODULE_PATH})

FIND_PACKAGE(Erlang REQUIRED)

include_directories(
  ${CMAKE_HOME_DIRECTORY}/include
  )
file(GLOB LANGERL_COMMON_SOURCES ${CMAKE_HOME_DIRECTORY}/src/*)

add_subdirectory (ruby)
