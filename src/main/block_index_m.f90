module block_index_m
  use jagged_array_m
  use contiguous_sets_m
  use node_data_m
  use jagged_array_3D_m
  implicit none
  private
  type, public :: block_index_c
    type(jagged_array_c), pointer :: parent, child
  contains
    procedure :: get_parent_num
    procedure :: get_child_num
  end type

  public :: create_block_index
  
contains

  function create_block_index(node_data, local_index) result(this)
    type(block_index_c), pointer :: this
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    type(jagged_array_c), pointer :: parent, child
    integer, pointer, contiguous :: local(:), parent_array(:), child_array(:)
    integer, allocatable :: lengths(:)
    integer :: node, num_node, array_ptr, parent_num, child_num, prev_parent, prev_child, i

    num_node = node_data%num_node
    allocate(lengths(num_node))

    lengths = 0
    do node=1, num_node-1
      prev_parent = 0
      local => local_index%get_array(node)
      prev_child = 0
      prev_parent = 0
      do i=1, size(local)
        parent_num = node_data%get_matrix_num(local(i))
        child_num = node_data%get_work_num(i, node)
        if(prev_parent /= parent_num .or. prev_child /= child_num)then
          if(prev_parent /= parent_num)then
            prev_parent = parent_num
          endif
          if(prev_child /= child_num)then
            prev_child = child_num
          endif
          lengths(node) = lengths(node)+1
        endif
      enddo
    enddo

    set => create_contiguous_sets(lengths)
    parent => create_jagged_array(set)
    child => create_jagged_array(set)

    do node=1, num_node-1
      parent_array => parent%get_array(node)
      child_array => child%get_array(node)
      local => local_index%get_array(node)
      prev_child = 0
      prev_parent = 0
      array_ptr = 1
      do i=1, size(local)
        parent_num = node_data%get_matrix_num(local(i))
        child_num = node_data%get_work_num(i, node)
        if(prev_parent /= parent_num .or. prev_child /= child_num)then
          if(prev_parent /= parent_num)then
            prev_parent = parent_num
          endif
          if(prev_child /= child_num)then
            prev_child = child_num
          endif
          parent_array(array_ptr) = parent_num
          child_array(array_ptr) = child_num
          array_ptr = array_ptr + 1
        endif
      enddo
    enddo

    allocate(this)
    this%parent => parent
    this%child => child

  end function

  integer function get_parent_num(this, node, idx) result(parent_num)
    class(block_index_c) :: this
    integer, intent(in) :: node, idx
    integer, pointer, contiguous :: ptr(:)

    ptr => this%parent%get_array(node)
    parent_num = ptr(idx)

  end function

  integer function get_child_num(this, node, idx) result(child_num)
    class(block_index_c) :: this
    integer, intent(in) :: node, idx
    integer, pointer, contiguous :: ptr(:)

    ptr => this%child%get_array(node)
    child_num = ptr(idx)

  end function

end module