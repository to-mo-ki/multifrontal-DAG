module assert_equal_m
  use log_m, only: disp_log
  use to_str_m, only: to_str
  implicit none
  private
  interface assert_equal
    module procedure assert_equal_int
    module procedure assert_equal_logical
    module procedure assert_equal_char
    module procedure assert_equal_DP
  end interface
  public :: assert_equal
  
contains
  subroutine assert_equal_int(message, answer, check)
    character(*), intent(in) :: message
    integer, intent(in) :: answer, check
    logical :: err_flag
  
    err_flag = answer /= check
    call disp_log(err_flag, message, to_str(answer), to_str(check))
  
  end subroutine
  
  subroutine assert_equal_logical(message, answer, check)
    character(*), intent(in) :: message
    logical, intent(in) :: answer, check
    logical :: err_flag
  
    err_flag = answer .neqv. check
    call disp_log(err_flag, message, to_str(answer), to_str(check))
  
  end subroutine
  
  subroutine assert_equal_char(message, answer, check)
    character(*), intent(in) :: message
    character(*), intent(in) :: answer, check
    logical :: err_flag
  
    err_flag = answer /= check
    call disp_log(err_flag, message, answer, check)
  end subroutine
  
  subroutine assert_equal_DP(message, answer, check, in_ignore_digits)
    character(*), intent(in) :: message
    double precision, intent(in) :: answer, check
    double precision, optional :: in_ignore_digits
    double precision :: ignore_digits
    logical :: err_flag
    
    if(.not. present(in_ignore_digits))then
      ignore_digits = 1.0E-10
    endif
  
    err_flag = abs(answer-check)/answer > ignore_digits
    call disp_log(err_flag, message, to_str(answer), to_str(check))
  
  end subroutine
end module