module seq_factorize_kernel_m
  use factors_m
  implicit none
  
contains

  subroutine factorize(factors, node, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: diag(:)
    integer :: n, info

    diag => factors%get_matrix_ptr(node, j, j)
    n = factors%get_block_size(j, node)
    call mydpotrf(n, diag, info)
        
  end subroutine

  subroutine solve(factors, node, i, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: diag(:), lower(:)
    integer :: n, m

    n = factors%get_block_size(j, node)
    m = factors%get_block_size(i, node)
    diag => factors%get_matrix_ptr(node, j, j)
    lower => factors%get_matrix_ptr(node, i, j)
    call mydtrsm(m, n, diag, solve)

  end subroutine

  subroutine sym_update(factors, node, i, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: off_diag(:), update_matrix(:)
    integer :: n, m

    n = factors%get_block_size(j, node)
    m = factors%get_block_size(i, node)
    off_diag => factors%get_matrix_ptr(node, i, j)
    update_matrix => factors%get_matrix_ptr(node, i, i)
    call mydsyrk(n, m, off_diag, update_matrix)

  end subroutine

  subroutine update(factors, node, i, j, k)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j, k
    double precision, pointer, contiguous :: lower(:), upper(:), update_matrix(:)
    integer :: n, m

    n = factors%get_block_size(j, node)
    m = factors%get_block_size(k, node)
    upper => factors%get_matrix_ptr(node, i, j)
    lower => factors%get_matrix_ptr(node, k, j)
    update_matrix => factors%get_matrix_ptr(node, k, i)
    call mydgemm(m, n, lower, upper, update)

  end subroutine
end module