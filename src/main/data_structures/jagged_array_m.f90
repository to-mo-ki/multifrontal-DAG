! ジャグ配列/多段階配列（要素数の異なる多次元配列）
module jagged_array_m
  use contiguous_sets_m
  implicit none
  private
  type, public :: jagged_array_c
    private
    type(contiguous_sets_c) :: set
    integer, pointer, contiguous :: val(:)
  contains
    procedure :: get_array
    procedure :: get_array_length
    procedure :: get_num_vals
    procedure :: get_num_arrays
  end type

  interface create_jagged_array
    module procedure :: create_jagged_array1
    module procedure :: create_jagged_array2
  end interface

  public :: create_jagged_array
  
contains
  type(jagged_array_c) function create_jagged_array1(num_length) result(this)
    integer :: num_length(:)

    this%set = create_contiguous_sets(num_length)
    allocate(this%val(this%set%get_num_elements()))

  end function

  type(jagged_array_c) function create_jagged_array2(num_length, val) result(this)
    integer :: num_length(:)
    integer, pointer, contiguous :: val(:)

    this%set = create_contiguous_sets(num_length)
    this%val => val

  end function

  function get_array(this, idx) result(ptr)
    class(jagged_array_c), target :: this
    integer, intent(in) :: idx
    integer, pointer, contiguous :: ptr(:)
    ptr => this%val(this%set%get_first(idx):this%set%get_last(idx))
  end function

  integer function get_array_length(this, idx) result(length)
    class(jagged_array_c) :: this
    integer, intent(in) :: idx
    length = this%set%get_length(idx)
  end function

  integer function get_num_vals(this) result(num_vals)
    class(jagged_array_c) :: this
    num_vals = size(this%val)
  end function

  integer function get_num_arrays(this) result(num_arrays)
    class(jagged_array_c) :: this
    num_arrays = this%set%get_num_sets()
  end function

end module