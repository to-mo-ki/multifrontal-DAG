module jagged_array_3D_m
  use contiguous_sets_m
  use jagged_array_m
  implicit none
  private
  type, public :: jagged_array_3D_c
    private
    type(contiguous_sets_c), pointer :: ptr
    type(jagged_array_c), pointer :: val
  contains
    procedure :: get_array
  end type

  public :: create_jagged_array_3D

contains

  function create_jagged_array_3D(ptr, val) result(this)
    type(contiguous_sets_c), pointer :: ptr
    type(jagged_array_c), pointer :: val
    type(jagged_array_3D_c), pointer :: this

    allocate(this)
    this%ptr => ptr
    this%val => val

  end function

  function get_array(this, i, j) result(array)
    class(jagged_array_3D_c) :: this
    integer, intent(in) :: i, j
    integer, pointer, contiguous :: array(:)
    integer :: idx

    idx = this%ptr%get_first(j)-1+i
    array => this%val%get_array(idx)

  end function

end module