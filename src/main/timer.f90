module timer_m
  implicit none
  private
  type, public :: timer_c
    character(:), pointer :: name
    double precision, private :: start_time, end_time
  contains
    procedure :: get_time
    procedure :: set_start_time
    procedure :: set_end_time
  end type
contains

  double precision function get_time(this) result(time)
    class(timer_c) :: this
    time = this%end_time - this%start_time
  end function get_time

  double precision function get_current_time() result(time)
    !$ use omp_lib
    time = omp_get_wtime()
  end function

  subroutine set_start_time(this)
    class(timer_c) :: this
    this%start_time = get_current_time()
  end subroutine

  subroutine set_end_time(this)
    class(timer_c) :: this
    this%end_time = get_current_time()
  end subroutine

end module