module matrix_extractor_m
  use node_data_m
  implicit none
  private
  type, abstract, public :: extractor_c
    type(node_data_c), pointer :: node_data
  contains
    private
    procedure, public :: get_ptr
    procedure(get_start_pos), deferred :: get_start_pos
    procedure(get_size), deferred :: get_size
    procedure(estimate_size), deferred, public :: estimate_size
  end type
  interface
    integer function get_start_pos(this, node, i, j)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node, i, j
    end function

    integer function get_size(this, node, i, j)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node, i, j
    end function

    integer function estimate_size(this, node)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node
    end function
  end interface

contains

  function get_ptr(this, array, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(extractor_c) :: this
    double precision, pointer, contiguous, intent(in) :: array(:)
    integer, intent(in) :: node, i, j
    integer :: length, start_pos

    start_pos = this%get_start_pos(node, i, j)
    length = this%get_size(node, i, j)
    ptr => array(start_pos:start_pos+length-1)

  end function
end module