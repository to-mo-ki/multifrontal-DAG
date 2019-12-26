module extend_add_subroutines_m
  use node_data_m
  use factors_m
  use jagged_array_3D_m
  use block_local_index_info_m
  use extend_add_kernel_m
  implicit none
  private
  public :: extend_add_ndiag, extend_add_diag

contains

  subroutine extend_add_ndiag(node_data, factors, block_local_index, block_local_index_info, i, j, cnode, pnode)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, intent(in) :: i, j, cnode, pnode
    integer, pointer, contiguous :: ilocal(:), jlocal(:)
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldp, ldc, roffset, coffset, pi, pj, ci, cj

    ci = block_local_index_info%get_child_num(cnode, i)
    cj = block_local_index_info%get_child_num(cnode, j)
    pi = block_local_index_info%get_parent_num(cnode, i)
    pj = block_local_index_info%get_parent_num(cnode, j)
    roffset = block_local_index_info%get_block_offset(cnode, i)
    coffset = block_local_index_info%get_block_offset(cnode, j)
    ilocal => block_local_index%get_array(cnode, i)
    jlocal => block_local_index%get_array(cnode, j)
    child_block => factors%get_work(cnode, ci, cj)
    parent_block => factors%get_matrix(pnode, pi, pj)
    ldp = node_data%get_matrix_block_size(pj, pnode)
    ldc = node_data%get_work_block_size(cj, cnode)
    call extend_add_rect(child_block, parent_block, jlocal, ilocal, ldc, ldp, coffset, roffset)
    
  end subroutine
  
  subroutine extend_add_diag(node_data, factors, block_local_index, block_local_index_info, j, cnode, pnode)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, intent(in) :: j, cnode, pnode
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldc, ldp, cj, pj, offset
    integer, pointer, contiguous :: local(:)
    
    cj = block_local_index_info%get_child_num(cnode, j)
    pj = block_local_index_info%get_parent_num(cnode, j)
    offset = block_local_index_info%get_block_offset(cnode, j)
    local => block_local_index%get_array(cnode, j)
    child_block => factors%get_work(cnode, cj, cj)
    parent_block => factors%get_matrix(pnode, pj, pj)
    ldp = node_data%get_matrix_block_size(pj, pnode)
    ldc = node_data%get_work_block_size(cj, cnode)
    call extend_add_tri(child_block, parent_block, local, ldc, ldp, offset)

  end subroutine

end module