cmake_minimum_required(VERSION 3.0)
project(NODERB)

FIND_PACKAGE(Ruby REQUIRED)
FIND_PACKAGE(Threads REQUIRED)
find_library(M_LIB m)
find_library(CRYPT_LIB crypt)
find_library(DL_LIB dl)

include_directories(${RUBY_INCLUDE_DIRS})

file(GLOB SOURCES src/*)

message(status "--> ${LANGERL_COMMON_SOURCES}")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_HOME_DIRECTORY}/bin)
add_executable(node.rb 
  ${SOURCES}
  ${LANGERL_COMMON_SOURCES}
  )
target_link_libraries(node.rb ${RUBY_LIBRARY})
target_link_libraries(node.rb ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(node.rb ${M_LIB})
target_link_libraries(node.rb ${CRYPT_LIB})
target_link_libraries(node.rb ${DL_LIB})
