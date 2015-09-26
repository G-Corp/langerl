cmake_minimum_required(VERSION 3.0)
project(LANGERL)

MACRO(ERL_LIST output)
  SET(var)
  FOREACH(it ${ARGN})
    GET_FILENAME_COMPONENT(outfile ${it} NAME_WE)
    SET(var "${var} '${outfile}'")
  ENDFOREACH(it)
  STRING(STRIP ${var} var)
  STRING(REPLACE " " "," ${output} ${var})
ENDMACRO(ERL_LIST)

MACRO(COMPILE_ERL output)
  SET(var)
  FOREACH(it ${ARGN})
    GET_FILENAME_COMPONENT(outfile ${it} NAME_WE)
    SET(beam "${CMAKE_HOME_DIRECTORY}/ebin/${outfile}.beam")
    add_custom_target("${outfile}.beam" ALL
      COMMAND ${ERLANG_COMPILE} ${it} 
      WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/ebin
      SOURCES ${it})
    set(var "${var} ${beam}")
  ENDFOREACH(it)
  set(${output} ${var})
ENDMACRO(COMPILE_ERL)

file(GLOB LANGERL_ERLANG_SOURCES ${CMAKE_HOME_DIRECTORY}/src/*.erl)

ERL_LIST(langerl_app_modules ${LANGERL_ERLANG_SOURCES})
SET(langerl_app_version "\"${LANGERL_VERSION}\"")
SET(langerl_app_name ${LANGERL_NAME})
configure_file(
  ${CMAKE_HOME_DIRECTORY}/src/${langerl_app_name}.app.src 
  ${CMAKE_HOME_DIRECTORY}/ebin/${langerl_app_name}.app
  )

COMPILE_ERL(beam_files ${LANGERL_ERLANG_SOURCES})

set_directory_properties(PROPERTIES 
  ADDITIONAL_MAKE_CLEAN_FILES ${beam_files}
  )

