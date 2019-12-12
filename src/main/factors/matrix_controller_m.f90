module matrix_controller_m
  use contiguous_sets_m
  implicit none
  private
  type, abstract, public :: matrix_controller_c
  contains
    procedure, public :: get_ptr
    procedure(get_start_pos), nopass, deferred :: get_start_pos
    procedure(get_size), nopass, deferred :: get_size
    procedure(estimate_size), nopass, deferred :: estimate_size
  end type
  interface
    integer function get_start_pos(nb, nc, nr, i, j)
      integer, intent(in) :: nb, nc, nr, i, j
    end function

    integer function get_size(nb, nc, nr, i, j)
      integer, intent(in) :: nb, nc, nr, i, j
    end function

    integer function estimate_size(nb, nc, nr)
      integer, intent(in) :: nb, nc, nr
    end function
  end interface
contains

  function get_ptr(this, array, nb, nc, nr, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(matrix_controller_c) :: this
    double precision, pointer, contiguous, intent(in) :: array(:)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: length, start_pos

    start_pos = this%get_start_pos(nb, nc, nr, i, j)
    length = this%get_size(nb, nc, nr, i, j)
    ptr => array(start_pos:start_pos+length-1)

  end function
end module