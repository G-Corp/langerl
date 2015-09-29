cmake_minimum_required(VERSION 3.0)
project(NODERB)

FIND_PACKAGE(Ruby REQUIRED)
IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  find_library(M_LIB m)
  find_library(CRYPT_LIB crypt)
  find_library(DL_LIB dl)
ENDIF()

include_directories(
  ${RUBY_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  )

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_HOME_DIRECTORY}/priv)
add_executable(node.ruby 
  ${SOURCES}
  ${LANGERL_COMMON_SOURCES}
  )
target_link_libraries(node.ruby ${RUBY_LIBRARY})
target_link_libraries(node.ruby ${LANGERL_COMMON_LIBRARIES})
IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  target_link_libraries(node.ruby ${M_LIB})
  target_link_libraries(node.ruby ${CRYPT_LIB})
  target_link_libraries(node.ruby ${DL_LIB})
ENDIF()
