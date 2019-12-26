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
    procedure :: get
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

  type(c_ptr) function get(this, i, j) result(ptr)
    class(jagged_array_cptr_c), target :: this
    integer, intent(in) :: i, j
    type(c_ptr), pointer, contiguous :: array(:)
    
    array => this%val(this%set%get_first(i):this%set%get_last(i))
    ptr = array(j)
  end function

end module