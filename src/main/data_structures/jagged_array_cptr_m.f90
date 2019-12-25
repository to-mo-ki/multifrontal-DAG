module jagged_array_cptr_m
  use contiguous_sets_m
  use iso_c_binding
  implicit none
  private
  type, public :: jagged_array_cptr_c
    private
    type(contiguous_sets_c), pointer :: set
    type(c_ptr), pointer, contiguous :: val(:)
  contains
    procedure :: get_array
  end type

  public :: create_jagged_array_cptr
  
contains
  function create_jagged_array_cptr(set) result(this)
    type(jagged_array_cptr_c), pointer :: this
    type(contiguous_sets_c), pointer :: set

    allocate(this)
    this%set => set
    allocate(this%val(set%get_num_elements()))

  end function

  function get_array(this, idx) result(ptr)
    class(jagged_array_cptr_c), target :: this
    integer, intent(in) :: idx
    type(c_ptr), pointer, contiguous :: ptr(:)
    ptr => this%val(this%set%get_first(idx):this%set%get_last(idx))
  end function

end module