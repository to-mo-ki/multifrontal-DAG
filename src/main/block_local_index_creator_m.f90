module block_local_index_creator_m
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use node_data_m
  implicit none
  private

  public :: create_num_blocks, create_num_indices, rebuild_val, create_over
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
          if(prev_parent /= parent_num)then
            prev_parent = parent_num
          endif
          if(prev_child /= child_num)then
            prev_child = child_num
          endif
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
    integer, pointer, contiguous :: local(:), array(:)
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
          if(prev_parent /= parent_num)then
            prev_parent = parent_num
          endif
          if(prev_child /= child_num)then
            prev_child = child_num
          endif
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
      local_index(i) = mod(local_index(i), nb)
      if(local_index(i) == 0)then
        local_index(i) = nb
      endif
    enddo

  end subroutine

  function create_over(node_set, set, block_local_index, nb) result(over)
    type(contiguous_sets_c), pointer :: node_set
    type(contiguous_sets_c), pointer :: set
    type(jagged_array_3D_c), pointer :: block_local_index
    integer, intent(in) :: nb
    type(jagged_array_c), pointer :: over
    integer, pointer, contiguous :: array(:)
    integer :: offset, node, num_node, i

    num_node = block_local_index%get_num_1d()
    over => create_jagged_array(set)
    do node=1, num_node
      array => over%get_array(node)
      ! supernode_size
      offset = mod(node_set%get_length(node), nb)
      do i=1, block_local_index%get_num_2d(node)
        offset = offset + block_local_index%get_num_3d(i, node)
        if(offset >= nb)then
          offset = offset - nb
          array(i) = offset
        else
          array(i) = -1
        endif
      enddo
    enddo

  end function

end module