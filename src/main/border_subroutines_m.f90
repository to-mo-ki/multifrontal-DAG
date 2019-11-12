module seq_border_kernel_m
  use factors_m
  use border_kernel_m
  implicit none
  
contains
  subroutine border_factorize(factors, node, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: diag(:)
    integer :: n, info, ssize, wsize

    supernode => factors%get_supernode_ptr(node, j, j)
    work => factors%get_work_ptr(node, j, j)
    call factors%get_border_info(node, ssize, wsize)
    call border_potrf(supernode, work, ssize, wsize)

  end subroutine

  subroutine border_solve(factors, node, i, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: diag_supernode(:), solve_supernode(:), solve_work(:)
    integer :: n, nrow

    
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
    integer :: n, nrow, ssize, wsize

    nrow = factors%get_block_size(i, node)
    call factors%get_border_info(node, ssize, wsize)
    rect => factors%get_supernode_ptr(node, i, j)
    diag => factors%get_matrix_ptr(node, i, i)
    call mydsyrk(ssize, nrow, rect, diag)

  end subroutine

  subroutine border_update(factors, node, i, j, k)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j, k
    double precision, pointer, contiguous :: lower(:), upper(:), update(:)
    integer :: n, m, upper_n, lower_n

    upper_n = factors%get_block_size(i, node)
    lower_n = factors%get_block_size(k, node)
    call factors%get_border_info(ssize, wsize)
    upper => factors%get_supernode_ptr(node, i, j)
    lower => factors%get_supernode_ptr(node, k, j)
    update => factors%get_matrix_ptr(node, k, i)
    call mydgemm(ssize, upper_n, lower_n, upper, lower, update)

  end subroutine
end module