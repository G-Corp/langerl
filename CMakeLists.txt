cmake_minimum_required(VERSION 3.0)

SET(MODS ${CMAKE_SOURCE_DIR}/CMake/erlang)
SET(CMAKE_MODULE_PATH ${MODS} ${CMAKE_MODULE_PATH})

FIND_PACKAGE(Erlang REQUIRED)

add_subdirectory (ruby)
