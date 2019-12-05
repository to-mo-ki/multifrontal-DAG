module factorize_subroutines_m
  use node_data_m
  use factors_m
  use factorize_kernel_m
  implicit none
  
contains

  subroutine factorize(node_data, factors, node, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: diag(:)
    integer :: n, info

    diag => factors%get_matrix_ptr(node, j, j)
    n = node_data%get_matrix_block_size(j, node)
    call mydpotrf(n, diag, info)
        
  end subroutine

  subroutine solve(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: diag(:), lower(:)
    integer :: n, m

    n = node_data%get_matrix_block_size(j, node)
    m = node_data%get_matrix_block_size(i, node)
    diag => factors%get_matrix_ptr(node, j, j)
    lower => factors%get_matrix_ptr(node, i, j)
    call mydtrsm(n, m, diag, lower)

  end subroutine

  subroutine sym_update(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: off_diag(:), update_matrix(:)
    integer :: n, m

    n = node_data%get_matrix_block_size(j, node)
    m = node_data%get_matrix_block_size(i, node)
    off_diag => factors%get_matrix_ptr(node, i, j)
    update_matrix => factors%get_matrix_ptr(node, i, i)
    call mydsyrk(m, n, off_diag, update_matrix)

  end subroutine

  subroutine update(node_data, factors, node, upper_idx, lower_idx, col_idx)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, upper_idx, lower_idx, col_idx
    double precision, pointer, contiguous :: lower(:), upper(:), update_matrix(:)
    integer :: upper_n, lower_n

    upper_n = node_data%get_matrix_block_size(upper_idx, node)
    lower_n = node_data%get_matrix_block_size(lower_idx, node)
    upper => factors%get_matrix_ptr(node, upper_idx, col_idx)
    lower => factors%get_matrix_ptr(node, lower_idx, col_idx)
    update_matrix => factors%get_matrix_ptr(node, lower_idx, upper_idx)
    call mydgemm(upper_n, lower_n, upper_n, lower, upper, update_matrix)

  end subroutine
end module