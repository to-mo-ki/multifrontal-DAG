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
  contains
    procedure :: get_local_index
    procedure :: get_start_row_num
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
    call rebuild_val(val, node_data%nb)
    
    this%node_ptr => set
    this%block_ptr => set2

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

end module