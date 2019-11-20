module block_local_index_m
  use jagged_array_m
  use contiguous_sets_m
  use jagged_array_3D_m
  implicit none
  private
  type, public :: block_local_index_c
    private
    type(jagged_array_3D_c), pointer :: block_local_index
    type(jagged_array_c), pointer :: block_num, over
  contains
    procedure :: get_local_index
    procedure :: get_block_nums
    procedure :: get_overs
  end type

  public :: create_block_local_index
  
contains

  function create_block_local_index(node_sets, local_index, nb) result(this)
    use block_local_index_creator_m
    type(contiguous_sets_c), pointer :: node_sets
    type(jagged_array_c), pointer :: local_index
    integer, intent(in) :: nb
    type(block_local_index_c), pointer :: this
    integer, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: set, set2
    type(jagged_array_c), pointer :: jag_2d
    allocate(this)

    set => create_num_blocks(local_index, nb)
    set2 => create_num_indices(local_index, local_index%get_set(), nb)
    jag_2d => create_jagged_array(set2, local_index%get_raw_val())
    this%block_local_index => create_jagged_array_3D(set, jag_2d)
    this%block_num => create_block_num(local_index, set, nb)
    val => local_index%get_raw_val()
    call rebuild_val(val, nb)
    this%over => create_over(node_sets, set, this%block_local_index, nb)

  end function
  
  function get_block_nums(this, node) result(block_nums)
    class(block_local_index_c) :: this
    integer, intent(in) :: node
    integer, pointer, contiguous :: block_nums(:)
    
    block_nums => this%block_num%get_array(node)

  end function

  function get_local_index(this, node, block_num) result(ptr)
    class(block_local_index_c) :: this
    integer, intent(in) :: node, block_num
    integer, pointer, contiguous :: ptr(:)

    ptr => this%block_local_index%get_array(block_num, node)
    
  end function

  function get_overs(this, node) result(overs)
    class(block_local_index_c) :: this
    integer, intent(in) :: node
    integer, pointer, contiguous :: overs(:)

    overs => this%over%get_array(node)

  end function

end module



  !function get_local_index(this, block_num, node_num, parent_block) result(ptr)
  !  type(block_local_index_c), pointer :: this
  !  integer, intent(in) :: block_num, node_num, parent_block
  !  integer, pointer, contiguous :: ptr_array
  !  integer :: block_start, block_end, first_block_size, nb, ptr_start, ptr_end
  !  
  !  first_block_size = this%node_data%get_border_work_size(node_num)
  !  nb = this%node_data%nb
  !  ptr_array => this%parent_block_ptr%get_array(node_num)
  !  offset = ptr_array(1)-1
!
  !  if(block_num == 1)then
  !    block_start = 1 + offset
  !    block_end = first_block_size + offset
  !  else
  !    block_start = (block_num-2)*nb+first_block_size + offset
  !    block_end = (block_num-1)*nb+first_block_size + offset
  !  endif
!
  !  if(block_num == 1)then
  !    ptr_start = block_start
  !  else
  !    ptr_start = ptr_array(block_num)
  !  endif
  !  
  !  if(block_num == size(ptr_array))then
  !    ptr_end = block_end
  !  else
  !    ptr_end = ptr_array(block_num+1)-1
  !  endif
  !  ptr => this%local_index(ptr_start, ptr_end)
!
  !end function
