! ジャグ配列/多段階配列（要素数の異なる多次元配列）
module jagged_array_m
  implicit none
  private
  type, public :: jagged_array_c
    private
    integer, pointer, contiguous :: ptr(:)
    integer, pointer, contiguous :: val(:)
  contains
    procedure :: get_array
    procedure :: get_array_length
    procedure :: get_num_vals
    procedure :: get_num_arrays
  end type

  public :: create_jagged_array
  
contains
  type(jagged_array_c) function create_jagged_array(ptr, val) result(this)
    integer, pointer, contiguous :: ptr(:)
    integer, pointer, contiguous :: val(:)

    this%ptr => ptr
    this%val => val

  end function

  function get_array(this, idx) result(ptr)
    class(jagged_array_c), target :: this
    integer, intent(in) :: idx
    integer, pointer, contiguous :: ptr(:)
    ptr => this%val(this%ptr(idx):this%ptr(idx+1)-1)
  end function

  integer function get_array_length(this, idx) result(length)
    class(jagged_array_c) :: this
    integer, intent(in) :: idx
    length = this%ptr(idx+1)-this%ptr(idx)
  end function

  integer function get_num_vals(this) result(num_vals)
    class(jagged_array_c) :: this
    num_vals = size(this%val)
  end function

  integer function get_num_arrays(this) result(num_arrays)
    class(jagged_array_c) :: this
    num_arrays = size(this%ptr) - 1
  end function

end module