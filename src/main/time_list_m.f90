module time_list_m
  implicit none
  integer, parameter :: ORDERING_TIME = 1
  integer, parameter :: ANALYZE_TIME = 2
  integer, parameter :: FACTORIZE_TIME = 3
  integer, parameter :: SOLVE_TIME = 4
  integer, parameter :: FORWARD_TIME = 5
  integer, parameter :: BACKWARD_TIME = 6
  integer, parameter :: REAL_FACTORIZE_TIME = 7
  integer, parameter :: NUM_TIMER = 7
end module