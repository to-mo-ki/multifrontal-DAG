module right_hand_m
  use contiguous_sets_m
  use jagged_array_m
  use block_arrays_m
  use array_extractor_m
  use supernode_array_extractor_m
  use work_array_extractor_m
  use border_array_extractor_m
  use node_data_m
  implicit none
  private
  type, public :: right_hand_c
    private
    type(node_data_c), pointer :: node_data
    type(block_arrays_c), pointer :: supernode, work, border
    integer :: nb
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
    class(extractor_c), pointer :: controller
    
    allocate(this)
    allocate(supernode_extractor_c::controller)
    this%supernode => create_block_arrays(nb, node_data%supernode_size, node_data%work_size, controller)
    allocate(work_extractor_c::controller)
    this%work => create_block_arrays(nb, node_data%supernode_size, node_data%work_size, controller)
    call this%work%set_zero()
    allocate(border_extractor_c::controller)
    this%border => create_block_arrays(nb, node_data%supernode_size, node_data%work_size, controller)
    call this%border%set_zero()
    this%nb = nb
    this%node_data => node_data
  
  end function

  subroutine set_val(this, val)
    class(right_hand_c) :: this
    double precision, contiguous, target :: val(:)

    call this%supernode%set_val(val)
    
  end subroutine

  function get_array_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(right_hand_c) :: this
    integer, intent(in) :: node, idx
    integer :: nc
    type(block_arrays_c), pointer :: block_arrays
    
    if(this%node_data%divisible(node))then
      nc = this%node_data%get_work_start_index(node)-1
      if(idx <= nc)then
        block_arrays => this%supernode
      else
        block_arrays => this%work
      endif
    else
      nc = this%node_data%get_work_start_index(node)
      if(idx < nc)then
        block_arrays => this%supernode
      else if(idx > nc)then
        block_arrays => this%work
      else
        block_arrays => this%border
      endif
    endif
    
    if(node == this%node_data%num_node)then
      block_arrays => this%supernode
    endif
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