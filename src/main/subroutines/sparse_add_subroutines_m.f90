module sparse_add_subroutines_m
  use sparse_add_kernel_m
  use factors_m
  use right_hand_m
  use block_local_index_m
  implicit none
  private

  public :: scatter_add, gather_add
  
contains
  subroutine scatter_add(factors, rh, block_local_index, j, cnode, pnode)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(block_local_index_c), pointer :: block_local_index
    integer, intent(in) :: j, cnode, pnode
    integer :: cj, pj, offset
    double precision, pointer, contiguous :: child_array(:), parent_array(:)
    integer, pointer, contiguous :: local(:)

    cj = block_local_index%get_child_num(cnode, j)
    pj = block_local_index%get_parent_num(cnode, j)
    offset = block_local_index%get_block_offset(cnode, j)
    local => block_local_index%get_local_index(cnode, j)
    child_array => rh%get_work_ptr(cnode, cj)
    parent_array => rh%get_array_ptr(pnode, pj)
    call scatter_add_kernel(child_array(offset+1:), local, parent_array, size(local))

  end subroutine

  subroutine gather_add(factors, rh, block_local_index, j, cnode, pnode)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(block_local_index_c), pointer :: block_local_index
    integer, intent(in) :: j, cnode, pnode
    integer :: cj, pj, offset
    double precision, pointer, contiguous :: child_array(:), parent_array(:)
    integer, pointer, contiguous :: local(:)

    cj = block_local_index%get_child_num(cnode, j)
    pj = block_local_index%get_parent_num(cnode, j)
    offset = block_local_index%get_block_offset(cnode, j)
    local => block_local_index%get_local_index(cnode, j)
    child_array => rh%get_work_ptr(cnode, cj)
    parent_array => rh%get_array_ptr(pnode, pj)
    call gather_add_kernel(parent_array, local, child_array(offset+1:), size(local))

  end subroutine

end module