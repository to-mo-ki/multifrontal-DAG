module border_subroutines_m
  use factors_m
  use border_kernel_m
  use factorize_kernel_m
  implicit none
  
contains
  subroutine border_factorize(factors, node, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: supernode(:), work(:)
    integer :: ssize, wsize

    supernode => factors%get_supernode_ptr(node, j, j)
    work => factors%get_work_ptr(node, j, j)
    call factors%get_border_info(node, ssize, wsize)
    call border_potrf(supernode, work, ssize, wsize)

  end subroutine

  subroutine border_solve(factors, node, i, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: diag_supernode(:), solve_supernode(:), solve_work(:)
    integer :: nrow, ssize, wsize
    
    nrow = factors%get_block_size(i, node)
    diag_supernode => factors%get_supernode_ptr(node, j, j)
    solve_supernode => factors%get_supernode_ptr(node, i, j)
    solve_work => factors%get_work_ptr(node, i, j)
    call factors%get_border_info(node, ssize, wsize)
    call border_trsm(diag_supernode, solve_supernode, solve_work, ssize, wsize, nrow)

  end subroutine

  subroutine border_sym_update(factors, node, i, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: rect(:), diag(:)
    integer :: nrow, ssize, wsize

    nrow = factors%get_block_size(i, node)
    call factors%get_border_info(node, ssize, wsize)
    rect => factors%get_supernode_ptr(node, i, j)
    diag => factors%get_matrix_ptr(node, i, i)
    call mydsyrk(nrow, ssize, rect, diag)

  end subroutine

  subroutine border_update(factors, node, upper_idx, lower_idx, col_idx)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, upper_idx, lower_idx, col_idx
    double precision, pointer, contiguous :: lower(:), upper(:), update(:)
    integer :: upper_n, lower_n, ssize, wsize

    upper_n = factors%get_block_size(upper_idx, node)
    lower_n = factors%get_block_size(lower_idx, node)
    call factors%get_border_info(node, ssize, wsize)
    upper => factors%get_supernode_ptr(node, upper_idx, col_idx)
    lower => factors%get_supernode_ptr(node, lower_idx, col_idx)
    update => factors%get_matrix_ptr(node, lower_idx, upper_idx)
    call mydgemm(ssize, lower_n, upper_n, lower, upper, update)

  end subroutine
end module