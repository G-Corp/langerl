cmake_minimum_required(VERSION 3.0)
project(LANGERL)

include("${LANGERL_SOURCE_DIR}/configuration.txt")

SET(MODS ${CMAKE_SOURCE_DIR}/CMake/erlang)
SET(CMAKE_MODULE_PATH ${MODS} ${CMAKE_MODULE_PATH})

FIND_PACKAGE(Erlang REQUIRED)
FIND_PACKAGE(Threads REQUIRED)

include_directories(
  ${CMAKE_HOME_DIRECTORY}/c_common
  ${ERLANG_EI_INCLUDE_PATH}
  ${ERLANG_ERTS_INCLUDE_PATH}
  )
link_directories(
  ${ERLANG_EI_LIBRARY_PATH}
  )
set(LANGERL_COMMON_LIBRARIES erl_interface ei ${CMAKE_THREAD_LIBS_INIT})
file(GLOB LANGERL_COMMON_SOURCES ${CMAKE_HOME_DIRECTORY}/c_common/*.c)

if($ENV{DEBUG}) 
  add_definitions(-DLANGERL_DEBUG)
endif($ENV{DEBUG}) 

add_subdirectory(ruby)
add_subdirectory(python)
add_subdirectory(complex)

