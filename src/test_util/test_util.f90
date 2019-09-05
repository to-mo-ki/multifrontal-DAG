module test_util
  !FIXME: ipo-cを行うとバグ発生
  use to_str_m, only: to_str
  use log_m
  use assert_equal_m
  use assert_equal_array_m
  implicit none

contains
  subroutine fail(message)
    character(*), intent(in) :: message
    call disp_log(.true., message)
  end subroutine


end module test_util
