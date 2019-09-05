set(CMAKE_Fortran_COMPILER "ifort")
set(CMAKE_Fortran_FLAGS_RELEASE "-qopenmp -ipo -mkl" CACHE STRING "" FORCE)
set(CMAKE_Fortran_FLAGS_DEBUG "-qopenmp -g -debug -mkl" CACHE STRING "" FORCE)

# ifortの最適化
if("${CMAKE_BUILD_TYPE}" STREQUAL "Release")
  set(CMAKE_Fortran_ARCHIVE_CREATE "ifort -ipo-c <OBJECTS> -o ipo_out.o" "<CMAKE_AR> rcs <TARGET> <LINK_FLAGS> ipo_out.o")
endif()

# StarPU
find_package(PkgConfig)
pkg_check_modules(STARPU starpu-1.3 REQUIRED)