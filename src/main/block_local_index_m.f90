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