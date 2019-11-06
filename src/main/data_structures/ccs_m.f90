module ccs_m
  use jagged_array_m
  use jagged_array_DP_m
  use contiguous_sets_m
  implicit none
  private
  type, public :: ccs_c
    type(jagged_array_c), pointer :: row
    type(jagged_array_DP_c), pointer :: val
  contains
    procedure :: get_row_array
    procedure :: get_val_array
  end type

  public :: create_ccs
  
contains
  function create_ccs(structure, val) result(this)
    type(ccs_c), pointer :: this
    type(jagged_array_c), pointer, intent(in) :: structure
    double precision, pointer, contiguous, intent(in) :: val(:)
    type(contiguous_sets_c), pointer :: ptr

    allocate(this)
    this%row => structure
    ptr => structure%get_set()
    this%val => create_jagged_array_DP(ptr, val)

  end function

  function get_row_array(this, idx) result(ptr)
    class(ccs_c), target :: this
    integer, intent(in) :: idx
    integer, pointer, contiguous :: ptr(:)

    ptr => this%row%get_array(idx)
    
  end function

  function get_val_array(this, idx) result(ptr)
    class(ccs_c), target :: this
    integer, intent(in) :: idx
    double precision, pointer, contiguous :: ptr(:)

    ptr => this%val%get_array(idx)
    
  end function

end module