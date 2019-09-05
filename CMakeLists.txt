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
add_executable(main main.f90)
target_include_directories(main PRIVATE ${STARPU_INCLUDE_DIRS})
target_link_libraries(main PRIVATE ${STARPU_LIBRARIES})
target_link_libraries(main PRIVATE mysolver)

file(GLOB_RECURSE test_utils RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "src/test_util/*.f90")
add_library(test_util STATIC ${test_utils})

file(GLOB_RECURSE test_mods RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "src/test_mod/*.f90")
add_library(test_mod STATIC ${test_mods})

file(GLOB_RECURSE test_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "src/test/*.f90")
set(test_lists)
foreach(test_source ${test_files})
  get_filename_component(test_filename ${test_source} NAME_WE)
  set(test_object ${test_filename}.out)
  add_executable(${test_object} ${test_source})
  target_include_directories(${test_object} PRIVATE ${STARPU_INCLUDE_DIRS})
  target_link_libraries(${test_object} PRIVATE ${STARPU_LIBRARIES})
  target_link_libraries(${test_object} PRIVATE mysolver test_util test_mod)
  add_dependencies(${test_object} mysolver test_util test_mod)
  list (APPEND test_lists ${test_object})
endforeach()
add_dependencies(main mysolver)

install(TARGETS mysolver ARCHIVE DESTINATION lib LIBRARY DESTINATION lib)
install(TARGETS ${test_lists} RUNTIME DESTINATION test)

find_package(MPI)
include_directories(${MPI_Fortran_INCLUDE_PATH})
message(STATUS "${MPI_Fortran_FOUND}")
message(STATUS "${MPI_Fortran_INCLUDE_PATH}")
message(STATUS "${MPI_Fortran_LIBRARIES}")


#BLAS/LAPACK


find_path (BLAS_INCLUDE_DIRS mkl.h #changed openblas_config.h ==> mkl.h
/usr/include
/usr/local/include
/usr/include/openblas
/opt/OpenBLAS/include
/usr/include/x86_64-linux-gnu
/opt/intel/compilers_and_libraries_2018.5.274/linux/mkl/include #added mkl include path
$ENV{BLAS_HOME}/include)

set (BLA_VENDOR Intel10_64lp_seq)
find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)