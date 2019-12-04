module right_hand_m
  use contiguous_sets_m
  use jagged_array_m
  use block_arrays_m
  use rh_controller_m
  use rh_supernode_controller_m
  use rh_work_controller_m
  use rh_border_controller_m
  use node_data_m
  implicit none
  private
  type, public :: right_hand_c
    private
    type(node_data_c), pointer :: node_data
    type(block_arrays_c), pointer :: supernode, work, border
    type(contiguous_sets_c), pointer :: node_sets
    type(jagged_array_c), pointer :: ccs
    integer :: nb
  contains
    procedure :: set_val
    procedure :: get_array_ptr
    procedure :: get_supernode_ptr
    procedure :: get_work_ptr
    procedure :: get_border_ptr
  end type

  public :: create_right_hand

contains
  function create_right_hand(node_data, node_sets, ccs, nb)result(this)
    type(right_hand_c), pointer :: this
    type(node_data_c), pointer :: node_data
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    type(jagged_array_c), pointer, intent(in) :: ccs
    integer, intent(in) :: nb
    class(rh_controller_c), pointer :: controller
    
    allocate(this)
    allocate(rh_supernode_controller_c::controller)
    this%supernode => create_block_arrays(nb, node_sets, ccs, controller)
    allocate(rh_work_controller_c::controller)
    this%work => create_block_arrays(nb, node_sets, ccs, controller)
    allocate(rh_border_controller_c::controller)
    this%border => create_block_arrays(nb, node_sets, ccs, controller)
    this%node_sets => node_sets
    this%ccs => ccs
    this%nb = nb
    this%node_data => node_data
  
  end function

  subroutine set_val(this, val)
    class(right_hand_c) :: this
    double precision, pointer, contiguous :: val(:)

    call this%supernode%set_val(val)
    
  end subroutine

  function get_array_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    integer :: nc
    type(block_arrays_c), pointer :: block_arrays

    nc = this%node_data%get_num_supernode_block(node)
    
    if(this%node_data%divisible(node))then
      if(idx <= nc)then
        block_arrays => this%supernode
      else
        block_arrays => this%work
      endif
    else
      if(idx < nc)then
        block_arrays => this%supernode
      else if(idx > nc)then
        block_arrays => this%work
      else
        block_arrays => this%border
      endif
    endif
    
    if(node == this%node_sets%get_num_sets())then
      block_arrays => this%supernode
    endif
    ptr => block_arrays%get_ptr(node, idx)
    
  end function

  function get_supernode_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    
    ptr => this%supernode%get_ptr(node, idx)
  end function

  function get_work_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    
    ptr => this%work%get_ptr(node, idx)

  end function

  function get_border_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    
    ptr => this%border%get_ptr(node, idx)

  end function

end module