module sparse_add_subroutines_m
  use sparse_add_kernel_m
  use factors_m
  use right_hand_m
  use jagged_array_3D_m
  use block_local_index_info_m
  implicit none
  private

  public :: scatter_add, gather
  
contains
  subroutine scatter_add(rh, block_local_index, block_local_index_info, j, cnode, pnode)
    type(right_hand_c), pointer :: rh
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, intent(in) :: j, cnode, pnode
    integer :: cj, pj, offset
    double precision, pointer, contiguous :: child_array(:), parent_array(:)
    integer, pointer, contiguous :: local(:)

    cj = block_local_index_info%get_child_num(cnode, j)
    pj = block_local_index_info%get_parent_num(cnode, j)
    offset = block_local_index_info%get_block_offset(cnode, j)
    local => block_local_index%get_array(cnode, j)
    child_array => rh%get_work(cnode, cj)
    parent_array => rh%get_array_ptr(pnode, pj)
    call scatter_add_kernel(child_array(offset+1:), local, parent_array, size(local))

  end subroutine

  subroutine gather(rh, block_local_index, block_local_index_info, j, cnode, pnode)
    type(right_hand_c), pointer :: rh
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, intent(in) :: j, cnode, pnode
    integer :: cj, pj, offset
    double precision, pointer, contiguous :: child_array(:), parent_array(:)
    integer, pointer, contiguous :: local(:)

    cj = block_local_index_info%get_child_num(cnode, j)
    pj = block_local_index_info%get_parent_num(cnode, j)
    offset = block_local_index_info%get_block_offset(cnode, j)
    local => block_local_index%get_array(cnode, j)
    child_array => rh%get_work(cnode, cj)
    parent_array => rh%get_array_ptr(pnode, pj)
    call gather_kernel(parent_array, local, child_array(offset+1:), size(local))

  end subroutine

end module