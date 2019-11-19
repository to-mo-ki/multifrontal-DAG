module block_local_index_creator_m
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use node_data_m
  implicit none
  private

  public :: create_num_blocks, create_block_num, create_num_indices, rebuild_val, create_state
contains

  function create_num_blocks(local_index, nb) result(parent_ptr)
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: parent_ptr
    integer, intent(in) :: nb
    integer, pointer, contiguous :: local(:)
    integer :: num_node, node, i, block_num, wsize, marker
    integer, allocatable :: num_blocks(:)

    num_node = local_index%get_num_arrays()

    allocate(num_blocks(num_node))
    num_blocks = 0
    do node=1, num_node-1
      marker = 0
      local => local_index%get_array(node)
      do i=1, size(local)
        block_num = (local(i)-1)/nb + 1
        if(marker /= block_num)then
          marker = block_num
          num_blocks(node) = num_blocks(node)+1
        endif
      enddo
    enddo

    parent_ptr => create_contiguous_sets(num_blocks)

  end function

  function create_block_num(local_index, set, nb) result(parent_block_num)
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    integer, intent(in) :: nb
    type(jagged_array_c), pointer :: parent_block_num
    integer, pointer, contiguous :: local(:), plocal(:)
    integer :: node, i, block_num, wsize, ptr, marker

    parent_block_num => create_jagged_array(set)

    marker = 0
    do node=1, local_index%get_num_arrays()-1
      local => local_index%get_array(node)
      plocal => parent_block_num%get_array(node)
      ptr = 0
      do i=1, size(local)
        block_num = (local(i)-1)/nb + 1
        if(marker /= block_num)then
          marker = block_num
          ptr = ptr+1
          plocal(ptr) = block_num
        endif
      enddo
    enddo

  end function

  function create_num_indices(local_index, set, nb) result(num_indices)
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    integer, intent(in) :: nb
    type(contiguous_sets_c), pointer :: num_indices
    integer, pointer, contiguous :: local(:), array(:)
    integer :: num_node, node, i, block_num, ptr, marker, prev_block_pos

    allocate(array(set%get_num_elements()))
    ptr = 0
    do node=1, local_index%get_num_arrays()-1
      local => local_index%get_array(node)
      marker = 1
      prev_block_pos = 0
      do i=1, size(local)-1
        block_num = (local(i+1)-1)/nb + 1
        if(marker /= block_num)then
          marker = block_num
          ptr = ptr+1
          array(ptr) = i-prev_block_pos
          prev_block_pos = i
        endif
      enddo
      ptr = ptr+1
      array(ptr) = size(local)-prev_block_pos
    enddo

    num_indices => create_contiguous_sets(array)

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

  function create_state(node_set, set, block_local_index, nb) result(state)
    type(contiguous_sets_c), pointer :: node_set
    type(contiguous_sets_c), pointer :: set
    type(jagged_array_3D_c), pointer :: block_local_index
    integer, intent(in) :: nb
    type(jagged_array_c), pointer :: state
    integer, pointer, contiguous :: array(:)
    integer :: offset, block_num, node, num_node, i

    num_node = block_local_index%get_num_1d()
    state => create_jagged_array(set)
    do node=1, num_node
      array => state%get_array(node)
      ! supernode_size
      offset = mod(node_set%get_length(node), nb)
      do i=1, block_local_index%get_num_2d(node)
        offset = offset + block_local_index%get_num_3d(i, node)
        if(offset > nb)then
          offset = offset - nb
          array(i) = 1
        else if(offset == nb)then
          offset = offset - nb
          array(i) = 2
        else
          array(i) = 0
        endif
      enddo
    enddo

  end function

end module