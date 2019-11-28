module extend_add_subroutines_m
  use factors_m
  use block_local_index_m
  use block_index_m
  use extend_add_kernel_m
  implicit none
  private
  public :: extend_add_ndiag, extend_add_diag

contains

  subroutine extend_add_ndiag(factors, block_local_index, block_index, i, j, cnode, pnode)
    type(factors_c), pointer :: factors
    type(block_local_index_c), pointer :: block_local_index
    type(block_index_c), pointer :: block_index
    integer, intent(in) :: i, j, cnode, pnode
    integer, pointer, contiguous :: ilocal(:), jlocal(:)
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldp, ldc, roffset, coffset, pi, pj, ci, cj

    ci = block_index%get_child_num(cnode, i)
    cj = block_index%get_child_num(cnode, j)
    pi = block_index%get_parent_num(cnode, i)
    pj = block_index%get_parent_num(cnode, j)
    roffset = block_local_index%get_block_offset(cnode, i)
    coffset = block_local_index%get_block_offset(cnode, j)
    ilocal => block_local_index%get_local_index(cnode, i)
    jlocal => block_local_index%get_local_index(cnode, j)
    child_block => factors%get_work_ptr(cnode, ci, cj)
    parent_block => factors%get_matrix_ptr(pnode, pi, pj)
    ldp = factors%get_block_size(pj, pnode)
    ldc = factors%get_work_size(cj, cnode)
    call extend_add_rect(child_block(roffset*ldc+coffset+1:), parent_block, jlocal, ilocal, ldc, ldp)
    
  end subroutine
  
  subroutine extend_add_diag(factors, block_local_index, block_index, j, cnode, pnode)
    type(factors_c), pointer :: factors
    type(block_local_index_c), pointer :: block_local_index
    type(block_index_c), pointer :: block_index
    integer, intent(in) :: j, cnode, pnode
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldc, ldp, cj, pj, offset
    integer, pointer, contiguous :: local(:)
    
    cj = block_index%get_child_num(cnode, j)
    pj = block_index%get_parent_num(cnode, j)
    offset = block_local_index%get_block_offset(cnode, j)
    local => block_local_index%get_local_index(cnode, j)
    child_block => factors%get_work_ptr(cnode, cj, cj)
    parent_block => factors%get_matrix_ptr(pnode, pj, pj)
    ldp = factors%get_block_size(pj, pnode)
    ldc = factors%get_work_size(cj, cnode)
    call extend_add_tri(child_block(offset*ldc+offset+1:), parent_block, local, ldc, ldp)

  end subroutine

end module