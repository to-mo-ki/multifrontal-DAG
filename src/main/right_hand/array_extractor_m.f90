module array_extractor_m
  use contiguous_sets_m
  use node_data_m
  implicit none
  private
  type, abstract, public :: extractor_c
    type(node_data_c), pointer :: node_data
  contains
    procedure, public :: get_ptr
    procedure(get_start_pos), deferred :: get_start_pos
    procedure(get_size), deferred :: get_size
    procedure(estimate_size), deferred :: estimate_size
  end type
  interface
    integer function get_start_pos(this, node, idx)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node, idx
    end function

    integer function get_size(this, node, idx)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node, idx
    end function

    integer function estimate_size(this, node)
      import extractor_c
      class(extractor_c) :: this
      integer, intent(in) :: node
    end function
  end interface
contains

  function get_ptr(this, array, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(extractor_c) :: this
    double precision, pointer, contiguous, intent(in) :: array(:)
    integer, intent(in) :: node, idx
    integer :: length, start_pos

    start_pos = this%get_start_pos(node, idx)
    length = this%get_size(node, idx)
    ptr => array(start_pos:start_pos+length-1)

  end function
end module