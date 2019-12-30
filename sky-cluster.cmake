set(CMAKE_Fortran_COMPILER "pgfortran")

set(CMAKE_Fortran_FLAGS "-mp=bind -lblas -llapack" CACHE STRING "" FORCE)
set(CMAKE_Fortran_FLAGS_RELEASE "" CACHE STRING "" FORCE)
set(CMAKE_Fortran_FLAGS_DEBUG "" CACHE STRING "" FORCE)

set(METIS_LIBRARY_DIRS "/import/mqhome/t-nakano/spack/opt/spack/linux-centos7-haswell/gcc-4.8.5/metis-5.1.0-nd4snpehdz6yy3d4ayyglaelqwat3xso/lib" CACHE STRING "" FORCE)
