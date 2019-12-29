module time_manager_m
  use timer_m
  use time_list_m, only: NUM_TIMER
  implicit none
  private
  type(timer_c) :: timers(NUM_TIMER)
  public :: start_time, end_time, get_time
contains
  subroutine start_time(num)
    integer, intent(in) :: num
    call timers(num)%set_start_time()
  end subroutine

  subroutine end_time(num)
    integer, intent(in) :: num
    call timers(num)%set_end_time()
  end subroutine

  double precision function get_time(num) result(res)
    integer, intent(in) :: num
    res = timers(num)%get_time()
  end function

end module