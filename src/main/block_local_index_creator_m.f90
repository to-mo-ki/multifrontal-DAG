module block_local_index_creator_m
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use node_data_m
  implicit none
  private

  public :: create_parent_ptr, create_parent_block_num, create_parent_block_ptr, rebuild_val
contains

  function create_parent_ptr(local_index, nb, max_num_block) result(parent_ptr)
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: parent_ptr
    integer, intent(in) :: nb, max_num_block
    integer, pointer, contiguous :: local(:)
    integer :: num_node, node, i, block_num, wsize
    integer, allocatable :: marker(:)
    integer, allocatable :: lengths(:)

    num_node = local_index%get_num_arrays()

    allocate(marker(max_num_block), lengths(num_node))
    marker = 0
    lengths = 0
    do node=1, num_node-1
      local => local_index%get_array(node)
      do i=1, size(local)
        block_num = (local(i)-1)/nb + 1
        if(marker(block_num) /= node)then
          marker(block_num) = node
          lengths(node) = lengths(node)+1
        endif
      enddo
    enddo

    parent_ptr => create_contiguous_sets(lengths)

  end function

  function create_parent_block_num(local_index, set, nb, max_num_block) result(parent_block_num)
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    integer, intent(in) :: nb, max_num_block
    type(jagged_array_c), pointer :: parent_block_num
    integer, pointer, contiguous :: local(:), plocal(:)
    integer :: num_node, node, i, block_num, wsize, ptr
    integer, allocatable :: marker(:)

    num_node = local_index%get_num_arrays()

    parent_block_num => create_jagged_array(set)
    allocate(marker(max_num_block))
    marker = 0
    do node=1, num_node-1
      local => local_index%get_array(node)
      plocal => parent_block_num%get_array(node)
      ptr = 0
      do i=1, size(local)
        block_num = (local(i)-1)/nb + 1
        if(marker(block_num) /= node)then
          marker(block_num) = node
          ptr = ptr+1
          plocal(ptr) = block_num
        endif
      enddo
    enddo

  end function

  function create_parent_block_ptr(local_index, set, nb, max_num_block) result(parent_block_ptr)
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: set
    integer, intent(in) :: nb, max_num_block
    type(jagged_array_c), pointer :: parent_block_ptr
    integer, pointer, contiguous :: local(:), plocal(:)
    integer :: num_node, node, i, block_num, wsize, offset, ptr
    integer, allocatable :: marker(:)

    num_node = local_index%get_num_arrays()

    parent_block_ptr => create_jagged_array(set)
    allocate(marker(max_num_block))

    marker = 0
    ptr = 0
    offset = 0
    do node=1, num_node-1
      local => local_index%get_array(node)
      plocal => parent_block_ptr%get_array(node)
      ptr = 0
      do i=1, size(local)
        block_num = (local(i)-1)/nb + 1
        if(marker(block_num) /= node)then
          marker(block_num) = node
          ptr = ptr+1
          plocal(ptr) = offset + i
        endif
      enddo
      offset = offset + size(local)
    enddo

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

end module