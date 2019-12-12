module extend_add_subroutines_m
  use node_data_m
  use factors_m
  use block_local_index_m
  use extend_add_kernel_m
  implicit none
  private
  public :: extend_add_ndiag, extend_add_diag

contains

  subroutine extend_add_ndiag(node_data, factors, block_local_index, i, j, cnode, pnode)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(block_local_index_c), pointer :: block_local_index
    integer, intent(in) :: i, j, cnode, pnode
    integer, pointer, contiguous :: ilocal(:), jlocal(:)
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldp, ldc, roffset, coffset, pi, pj, ci, cj

    ci = block_local_index%get_child_num(cnode, i)
    cj = block_local_index%get_child_num(cnode, j)
    pi = block_local_index%get_parent_num(cnode, i)
    pj = block_local_index%get_parent_num(cnode, j)
    roffset = block_local_index%get_block_offset(cnode, i)
    coffset = block_local_index%get_block_offset(cnode, j)
    ilocal => block_local_index%get_local_index(cnode, i)
    jlocal => block_local_index%get_local_index(cnode, j)
    child_block => factors%get_work(cnode, ci, cj)
    parent_block => factors%get_matrix(pnode, pi, pj)
    ldp = node_data%get_matrix_block_size(pj, pnode)
    ldc = node_data%get_work_block_size(cj, cnode)
    call extend_add_rect(child_block(roffset*ldc+coffset+1:), parent_block, jlocal, ilocal, ldc, ldp)
    
  end subroutine
  
  subroutine extend_add_diag(node_data, factors, block_local_index, j, cnode, pnode)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(block_local_index_c), pointer :: block_local_index
    integer, intent(in) :: j, cnode, pnode
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldc, ldp, cj, pj, offset
    integer, pointer, contiguous :: local(:)
    
    cj = block_local_index%get_child_num(cnode, j)
    pj = block_local_index%get_parent_num(cnode, j)
    offset = block_local_index%get_block_offset(cnode, j)
    local => block_local_index%get_local_index(cnode, j)
    child_block => factors%get_work(cnode, cj, cj)
    parent_block => factors%get_matrix(pnode, pj, pj)
    ldp = node_data%get_matrix_block_size(pj, pnode)
    ldc = node_data%get_work_block_size(cj, cnode)
    call extend_add_tri(child_block(offset*ldc+offset+1:), parent_block, local, ldc, ldp)

  end subroutine

end module