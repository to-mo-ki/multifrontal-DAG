module test_util
  !FIXME: ipo-cを行うとバグ発生
  use to_str_m, only: to_str
  use log_m
  use assert_equal_m
  use assert_equal_array_m
  use tests_m
  implicit none
  interface assert_equal
    module procedure assert_equal_int
    module procedure assert_equal_big_int
    module procedure assert_equal_logical
    module procedure assert_equal_char
    module procedure assert_equal_DP
    module procedure assert_equal_array_DP
    module procedure assert_equal_array_DP2
    module procedure assert_equal_array_int
    module procedure assert_equal_array_logical
  end interface

contains
  subroutine fail(message)
    character(*), intent(in) :: message
    call disp_log(.true., message)
  end subroutine


end module test_util
