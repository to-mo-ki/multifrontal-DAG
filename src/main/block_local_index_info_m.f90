module block_local_index_info_m
  use jagged_array_m
  use contiguous_sets_m
  use jagged_array_3D_m
  use jagged_array_cptr_m
  use node_data_m
  implicit none
  private
  type, public :: block_local_index_info_c
    private
    type(contiguous_sets_c), pointer, public :: node_ptr
    type(contiguous_sets_c), pointer :: block_ptr
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: parent, child
    type(jagged_array_c), pointer :: local_index
  contains
    procedure :: create_block_local_index
    procedure :: get_start_row_num
    procedure :: get_block_offset
    procedure :: get_parent_num
    procedure :: get_child_num
    procedure :: get_num_block
  end type

  public :: create_block_local_index_info
  
contains

  function create_block_local_index_info(node_data, local_index) result(this)
    use block_local_index_info_creator_m
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(block_local_index_info_c), pointer :: this
    allocate(this)

    this%node_ptr => create_num_blocks(node_data, local_index)
    this%block_ptr => create_num_indices(node_data, local_index, this%node_ptr%get_num_elements())
    call create_block_nums(node_data, local_index, this%node_ptr, this%parent, this%child)
    this%node_data => node_data
    this%local_index => local_index
    
  end function

  function create_block_local_index(this) result(block_local_index)
    use block_local_index_info_creator_m
    class(block_local_index_info_c) :: this
    type(jagged_array_3D_c), pointer :: block_local_index
    integer, pointer, contiguous :: val(:)
    type(jagged_array_c), pointer :: jag_2d
    
    val => this%local_index%get_raw_val()
    jag_2d => create_jagged_array(this%block_ptr, val)
    block_local_index => create_jagged_array_3D(this%node_ptr, jag_2d)
    call rebuild_val(val, this%node_data%nb)

  end function

  integer function get_start_row_num(this, node, idx) result(row_num)
    class(block_local_index_info_c) :: this
    integer, intent(in) :: node, idx
    integer :: first_block_idx, block_idx, first_block_pos, block_pos

    first_block_idx = this%node_ptr%get_first(node)
    block_idx = first_block_idx - 1 + idx
    first_block_pos = this%block_ptr%get_first(first_block_idx)
    block_pos = this%block_ptr%get_first(block_idx)
    row_num = block_pos - first_block_pos+1

  end function

  integer function get_block_offset(this, node, idx) result(offset)
    class(block_local_index_info_c) :: this
    integer, intent(in) :: node, idx
    integer :: ssize, wsize

    ssize = this%node_data%border_supernode_size(node)
    wsize = this%node_data%border_work_size(node)

    if(this%get_start_row_num(node, idx) <= wsize)then !error
      offset = this%get_start_row_num(node, idx)-1
    else
      offset = mod(ssize+this%get_start_row_num(node, idx)-1, this%node_data%nb)
    endif

  end function

  integer function get_parent_num(this, node, idx) result(parent_num)
    class(block_local_index_info_c) :: this
    integer, intent(in) :: node, idx
    integer, pointer, contiguous :: ptr(:)

    ptr => this%parent%get_array(node)
    parent_num = ptr(idx)

  end function

  integer function get_child_num(this, node, idx) result(child_num)
    class(block_local_index_info_c) :: this
    integer, intent(in) :: node, idx
    integer, pointer, contiguous :: ptr(:)

    ptr => this%child%get_array(node)
    child_num = ptr(idx)

  end function

  integer function get_num_block(this, node) result(num_block)
    class(block_local_index_info_c) :: this
    integer :: node

    num_block = this%child%get_array_length(node)

  end function

end module