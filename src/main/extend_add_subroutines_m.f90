module extend_add_subroutines_m
  use factors_m
  use extend_add_kernel_m
  implicit none
  private
  public :: extend_add_ndiag, extend_add_diag

contains

  ! OPTIMIZE: get_matrix_ptr => get_work_ptr, get_block_sizeも変更
  subroutine extend_add_ndiag(factors, i, j, pi, pj, cnode, pnode, ilocal, jlocal, roffset, coffset)
    type(factors_c), pointer :: factors
    integer, intent(in) :: i, j, pi, pj, cnode, pnode, roffset, coffset
    integer, contiguous :: ilocal(:), jlocal(:)
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldp, ldc

    child_block => factors%get_matrix_ptr(cnode, i, j)
    parent_block => factors%get_matrix_ptr(pnode, pi, pj)
    ldp = factors%get_block_size(pj, pnode)
    ldc = factors%get_block_size(j, cnode)
    call extend_add_rect(child_block(roffset*ldc+coffset+1:), parent_block, jlocal, ilocal, ldc, ldp)
    
  end subroutine
  
  subroutine extend_add_diag(factors, j, pj, cnode, pnode, local, offset)
    type(factors_c), pointer :: factors
    integer, intent(in) :: j, pj, cnode, pnode, offset
    integer, contiguous :: local(:)
    double precision, pointer, contiguous :: parent_block(:), child_block(:)
    integer :: ldc, ldp
    
    child_block => factors%get_matrix_ptr(cnode, j, j)
    parent_block => factors%get_matrix_ptr(pnode, pj, pj)
    ldp = factors%get_block_size(pj, pnode)
    ldc = factors%get_block_size(j, cnode)
    call extend_add_tri(child_block(offset*ldc+offset+1:), parent_block, local, ldc, ldp)

  end subroutine

end module