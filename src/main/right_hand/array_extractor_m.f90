module extractor_m
  use contiguous_sets_m
  implicit none
  private
  type, abstract, public :: extractor_c
  contains
    procedure, public :: get_ptr
    procedure(get_start_pos), nopass, deferred :: get_start_pos
    procedure(get_size), nopass, deferred :: get_size
    procedure(estimate_size), nopass, deferred :: estimate_size
  end type
  interface
    integer function get_start_pos(nb, nc, nr, idx)
      integer, intent(in) :: nb, nc, nr, idx
    end function

    integer function get_size(nb, nc, nr, idx)
      integer, intent(in) :: nb, nc, nr, idx
    end function

    integer function estimate_size(nb, nc, nr)
      integer, intent(in) :: nb, nc, nr
    end function
  end interface
contains

  function get_ptr(this, array, nb, nc, nr, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(extractor_c) :: this
    double precision, pointer, contiguous, intent(in) :: array(:)
    integer, intent(in) :: nb, nc, nr, idx
    integer :: length, start_pos

    start_pos = this%get_start_pos(nb, nc, nr, idx)
    length = this%get_size(nb, nc, nr, idx)
    ptr => array(start_pos:start_pos+length-1)

  end function
end module