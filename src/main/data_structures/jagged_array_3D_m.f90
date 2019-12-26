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
    procedure :: get_num_1d
    procedure :: get_num_2d
    procedure :: get_num_3d
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

    idx = this%ptr%get_first(i)-1+j
    array => this%val%get_array(idx)

  end function

  integer function get_num_1d(this) result(num_1d)
    class(jagged_array_3D_c) :: this

    num_1d = this%ptr%get_num_sets()

  end function

  integer function get_num_2d(this, idx) result(num_2d)
    class(jagged_array_3D_c) :: this
    integer, intent(in) :: idx
    
    num_2d = this%ptr%get_length(idx)
    
  end function

  integer function get_num_3d(this, i, j) result(num_3d)
    class(jagged_array_3D_c) :: this
    integer, intent(in) :: i, j
    integer :: idx
    
    idx = this%ptr%get_first(i)-1+j
    num_3d = this%val%get_array_length(idx)
    
  end function

end module