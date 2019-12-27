module options_m
  implicit none
  integer, parameter :: OPTION_NB = 1
  integer, parameter :: OPTION_MAX_ZERO = 2
  integer, parameter :: OPTION_USE_STARPU = 3
  integer, parameter :: NUM_OPTIONS = 3
  integer, allocatable :: options(:)

end module