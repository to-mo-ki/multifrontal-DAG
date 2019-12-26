module starpu_factorize_subroutines_m
  use node_data_m
  use starpu_factors_m
  use factorize_tasks_m
  use iso_c_binding
  implicit none
  private

  public :: factorize, solve, sym_update, update
  
contains

  subroutine factorize(node_data, factors, node, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, j
    type(c_ptr), pointer :: diag
    integer :: n, info

    diag => factors%get_matrix(node, j, j)
    n = node_data%get_matrix_block_size(j, node)
    call factorize_task%insert_task((/n/), (/diag/))
        
  end subroutine

  subroutine solve(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    type(c_ptr), pointer :: diag, lower
    integer :: n, m

    n = node_data%get_matrix_block_size(j, node)
    m = node_data%get_matrix_block_size(i, node)
    diag => factors%get_matrix(node, j, j)
    lower => factors%get_matrix(node, i, j)
    call solve_task%insert_task((/n, m/), (/diag, lower/))

  end subroutine

  subroutine sym_update(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    type(c_ptr), pointer :: off_diag, update_matrix
    integer :: n, m

    n = node_data%get_matrix_block_size(j, node)
    m = node_data%get_matrix_block_size(i, node)
    off_diag => factors%get_matrix(node, i, j)
    update_matrix => factors%get_matrix(node, i, i)
    call sym_update_task%insert_task((/m, n/), (/off_diag, update_matrix/))

  end subroutine

  subroutine update(node_data, factors, node, upper_idx, lower_idx, col_idx)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, upper_idx, lower_idx, col_idx
    type(c_ptr), pointer :: lower, upper, update_matrix
    integer :: upper_n, lower_n

    upper_n = node_data%get_matrix_block_size(upper_idx, node)
    lower_n = node_data%get_matrix_block_size(lower_idx, node)
    upper => factors%get_matrix(node, upper_idx, col_idx)
    lower => factors%get_matrix(node, lower_idx, col_idx)
    update_matrix => factors%get_matrix(node, lower_idx, upper_idx)
    call update_task%insert_task((/upper_n, lower_n, upper_n/), (/lower, upper, update_matrix/))
    
  end subroutine
end module