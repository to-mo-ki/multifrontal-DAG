module jagged_array_DP_m
  use contiguous_sets_m
  implicit none
  private
  type, public :: jagged_array_DP_c
    private
    type(contiguous_sets_c), pointer :: set
    double precision, pointer, contiguous :: val(:)
  contains
    procedure :: get_array
  end type

  public :: create_jagged_array_DP
  
contains
  function create_jagged_array_DP(set, val) result(this)
    type(jagged_array_DP_c), pointer :: this
    type(contiguous_sets_c), pointer :: set
    double precision, target, contiguous :: val(:)

    allocate(this)
    this%set => set
    this%val => val

  end function

  function get_array(this, idx) result(ptr)
    class(jagged_array_DP_c), target :: this
    integer, intent(in) :: idx
    double precision, pointer, contiguous :: ptr(:)
    ptr => this%val(this%set%get_first(idx):this%set%get_last(idx))
  end function

end module