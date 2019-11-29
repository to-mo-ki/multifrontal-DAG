module block_local_index_m
  use jagged_array_m
  use contiguous_sets_m
  use jagged_array_3D_m
  use node_data_m
  implicit none
  private
  type, public :: block_local_index_c
    private
    type(contiguous_sets_c), pointer :: node_ptr, block_ptr
    type(jagged_array_3D_c), pointer :: block_local_index
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: parent, child
  contains
    procedure :: get_local_index
    procedure :: get_start_row_num
    procedure :: get_block_offset
    procedure :: get_parent_num
    procedure :: get_child_num
  end type

  public :: create_block_local_index
  
contains

  function create_block_local_index(node_data, local_index) result(this)
    use block_local_index_creator_m
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(block_local_index_c), pointer :: this
    integer, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: set, set2
    type(jagged_array_c), pointer :: jag_2d
    allocate(this)

    set => create_num_blocks(node_data, local_index)
    set2 => create_num_indices(node_data, local_index, set%get_num_elements())
    jag_2d => create_jagged_array(set2, local_index%get_raw_val())
    this%block_local_index => create_jagged_array_3D(set, jag_2d)
    val => local_index%get_raw_val()
    call create_block_nums(node_data, local_index, set, this%parent, this%child)
    call rebuild_val(val, node_data%nb)
    
    this%node_ptr => set
    this%block_ptr => set2
    this%node_data => node_data
    
  end function

  function get_local_index(this, node, block_num) result(ptr)
    class(block_local_index_c) :: this
    integer, intent(in) :: node, block_num
    integer, pointer, contiguous :: ptr(:)

    ptr => this%block_local_index%get_array(block_num, node)
    
  end function

  integer function get_start_row_num(this, node, idx) result(row_num)
    class(block_local_index_c) :: this
    integer, intent(in) :: node, idx
    integer :: first_block_idx, block_idx, first_block_pos, block_pos

    first_block_idx = this%node_ptr%get_first(node)
    block_idx = first_block_idx - 1 + idx
    first_block_pos = this%block_ptr%get_first(first_block_idx)
    block_pos = this%block_ptr%get_first(block_idx)
    row_num = block_pos - first_block_pos+1

  end function

  ! TODO:TEST
  integer function get_block_offset(this, node, idx) result(offset)
    class(block_local_index_c) :: this
    integer, intent(in) :: node, idx
    integer :: ssize, wsize

    ssize = this%node_data%get_border_supernode_size(node)
    wsize = this%node_data%get_border_work_size(node)

    if(ssize+this%get_start_row_num(node, idx)-1 <= wsize)then
      offset = this%get_start_row_num(node, idx)-1
    else
      offset = mod(ssize+this%get_start_row_num(node, idx)-1, this%node_data%nb)
    endif

  end function

  integer function get_parent_num(this, node, idx) result(parent_num)
    class(block_local_index_c) :: this
    integer, intent(in) :: node, idx
    integer, pointer, contiguous :: ptr(:)

    ptr => this%parent%get_array(node)
    parent_num = ptr(idx)

  end function

  integer function get_child_num(this, node, idx) result(child_num)
    class(block_local_index_c) :: this
    integer, intent(in) :: node, idx
    integer, pointer, contiguous :: ptr(:)

    ptr => this%child%get_array(node)
    child_num = ptr(idx)

  end function

end module