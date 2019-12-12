cmake_minimum_required(VERSION 3.5.0)
 
#プロジェクト名
project(hello Fortran)

enable_language(Fortran)
 
#ディレクトリ設定
set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)


message(STATUS "Using toolchain file: ${CMAKE_TOOLCHAIN_FILE}.")
message(STATUS "install directory: ${CMAKE_INSTALL_PREFIX}")
file(GLOB_RECURSE SOURCES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "src/main/*.f90" "src/starpu/*.f90")
add_library(
  mysolver
  STATIC
  ${SOURCES}
)

link_directories(${STARPU_LIBRARY_DIRS})
find_library(METIS_LIBRARY metis PATHS ${METIS_LIBRARY_DIRS})
if(METIS_LIBRARY)
  message(STATUS "Library metis found in ${METIS_LIBRARY_DIRS}")
else()
  message(FATAL_ERROR "Library metis not found in ${METIS_LIBRARY_DIRS}")
endif()
add_executable(main main.f90)
# install時のrpathを設定
set_target_properties(main PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
target_link_libraries(main PRIVATE ${STARPU_LIBRARIES})
target_link_libraries(main PRIVATE mysolver)
target_link_libraries(main PUBLIC ${METIS_LIBRARY})

file(GLOB_RECURSE test_utils RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "src/test_util/*.f90")
add_library(test_util STATIC ${test_utils})

file(GLOB_RECURSE test_mods RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "src/test_mod/*.f90")
add_library(test_mod STATIC ${test_mods})
add_dependencies(test_mod mysolver test_util)

file(GLOB_RECURSE test_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "src/test/*.f90")
set(test_lists)
foreach(test_source ${test_files})
  get_filename_component(test_filename ${test_source} NAME_WE)
  set(test_object ${test_filename}.out)
  add_executable(${test_object} ${test_source})
  target_include_directories(${test_object} PRIVATE ${STARPU_INCLUDE_DIRS})
  target_link_libraries(${test_object} PRIVATE ${STARPU_LIBRARIES})
  # NOTE: リンクの順番は大事
  target_link_libraries(${test_object} PRIVATE test_mod mysolver test_util)
  target_link_libraries(${test_object} PUBLIC ${METIS_LIBRARY})
  set_target_properties(${test_object} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)
  add_dependencies(${test_object} mysolver test_util test_mod)
  list (APPEND test_lists ${test_object})
endforeach()
add_dependencies(main mysolver)

install(TARGETS mysolver ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
install(TARGETS ${test_lists} DESTINATION test)
install(TARGETS main DESTINATION ./)
