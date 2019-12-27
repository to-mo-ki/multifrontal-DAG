module right_hand_m
  use contiguous_sets_m
  use jagged_array_m
  use block_arrays_m
  use node_data_m
  use extractor_types_m
  implicit none
  private
  type, public :: right_hand_c
    private
    type(node_data_c), pointer :: node_data
    type(block_arrays_c), pointer :: supernode, work, border
  contains
    procedure :: set_val
    procedure :: get_array_ptr
    procedure :: get_supernode
    procedure :: get_work
    procedure :: get_border
  end type

  public :: create_right_hand

contains
  function create_right_hand(node_data, nb)result(this)
    type(right_hand_c), pointer :: this
    type(node_data_c), pointer :: node_data
    integer, intent(in) :: nb
    
    allocate(this)
    this%supernode => create_block_arrays(node_data, SUPERNODE_EXTRACTOR)
    this%work => create_block_arrays(node_data, WORK_EXTRACTOR)
    this%border => create_block_arrays(node_data, BORDER_EXTRACTOR)
    this%node_data => node_data
  
  end function

  subroutine set_val(this, val)
    class(right_hand_c) :: this
    double precision, contiguous, target :: val(:)

    call this%border%set_zero()
    call this%work%set_zero()
    call this%supernode%set_val(val)
    
  end subroutine

  function get_array_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    integer :: nc
    type(block_arrays_c), pointer :: block_arrays
    
    select case(this%node_data%get_extractor_type(idx, node))
    case(SUPERNODE_EXTRACTOR)
      block_arrays => this%supernode
    case(BORDER_EXTRACTOR)
      block_arrays => this%border
    case(WORK_EXTRACTOR)
      block_arrays => this%work
  end select
    ptr => block_arrays%get_ptr(node, idx)
    
  end function

  function get_supernode(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    
    ptr => this%supernode%get_ptr(node, idx)
  end function

  function get_work(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    
    ptr => this%work%get_ptr(node, idx)

  end function

  function get_border(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    
    ptr => this%border%get_ptr(node, idx)

  end function

end module