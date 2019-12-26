module block_local_index_info_creator_m
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use node_data_m
  use integer_function_m
  implicit none
  private

  public :: create_num_blocks, create_num_indices, rebuild_val, create_block_nums
contains

  function create_num_blocks(node_data, local_index) result(set)
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    integer, pointer, contiguous :: local(:)
    integer, allocatable :: num_blocks(:)
    integer :: nb, node, num_node, parent_num, child_num, prev_parent, prev_child, i

    nb = node_data%nb
    num_node = node_data%num_node
    allocate(num_blocks(num_node))

    num_blocks = 0
    do node=1, num_node-1
      prev_parent = 0
      local => local_index%get_array(node)
      prev_child = 0
      prev_parent = 0
      do i=1, size(local)
        parent_num = node_data%get_matrix_num(local(i))
        child_num = node_data%get_work_num(i, node)
        if(prev_parent /= parent_num .or. prev_child /= child_num)then
          prev_parent = parent_num
          prev_child = child_num
          num_blocks(node) = num_blocks(node)+1
        endif
      enddo
    enddo

    set => create_contiguous_sets(num_blocks)

  end function

  function create_num_indices(node_data, local_index, sum_blocks) result(set)
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    integer, intent(in) :: sum_blocks
    integer, pointer, contiguous :: local(:)
    integer, allocatable :: num_indices(:)
    integer :: node, num_node, array_ptr, parent_num, child_num, prev_parent, prev_child, i

    num_node = local_index%get_num_arrays()

    allocate(num_indices(sum_blocks))
    num_indices = 0
    array_ptr = 0
    do node=1, num_node-1
      local => local_index%get_array(node)
      prev_child = 0
      prev_parent = 0
      do i=1, size(local)
        parent_num = node_data%get_matrix_num(local(i))
        child_num = node_data%get_work_num(i, node)
        if(prev_parent /= parent_num .or. prev_child /= child_num)then
          prev_parent = parent_num
          prev_child = child_num
          array_ptr = array_ptr + 1
        endif
        num_indices(array_ptr) = num_indices(array_ptr) + 1
      enddo
    enddo

    set => create_contiguous_sets(num_indices)

  end function

  subroutine rebuild_val(local_index, nb)
    integer, contiguous, intent(inout) :: local_index(:)
    integer, intent(in) :: nb
    integer :: i
    
    do i=1, size(local_index)
      local_index(i) = mod2(local_index(i), nb)
    enddo

  end subroutine

  subroutine create_block_nums(node_data, local_index, set, parent, child)
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    type(jagged_array_c), pointer :: parent, child
    integer, pointer, contiguous :: local(:), parent_array(:), child_array(:)
    integer :: node, array_ptr, parent_num, child_num, prev_parent, prev_child, i

    parent => create_jagged_array(set)
    child => create_jagged_array(set)

    do node=1, node_data%num_node-1
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
          prev_parent = parent_num
          prev_child = child_num
          parent_array(array_ptr) = parent_num
          child_array(array_ptr) = child_num
          array_ptr = array_ptr + 1
        endif
      enddo
    enddo

  end subroutine

end module