module starpu_border_subroutines_m
  use node_data_m
  use starpu_factors_m
  use starpu_border_kernel_m
  use factorize_tasks_m
  use border_tasks_m
  use iso_c_binding
  implicit none
  
contains
  subroutine border_factorize(node_data, factors, node, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, j
    type(c_ptr), pointer :: supernode, work
    integer :: ssize, wsize

    supernode => factors%get_supernode(node, j, j)
    work => factors%get_work(node, j, j)
    ssize = node_data%border_supernode_size(node)
    wsize = node_data%border_work_size(node)
    call border_factorize_task%insert_task((/ssize, wsize/), (/supernode, work/))

  end subroutine

  subroutine border_solve(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    type(c_ptr), pointer :: diag_supernode, solve_supernode, solve_work
    integer :: nrow, ssize, wsize
    
    nrow = node_data%get_matrix_block_size(i, node)
    diag_supernode => factors%get_supernode(node, j, j)
    solve_supernode => factors%get_supernode(node, i, j)
    solve_work => factors%get_work(node, i, j)
    ssize = node_data%border_supernode_size(node)
    wsize = node_data%border_work_size(node)
    call border_solve_task%insert_task((/ssize, wsize, nrow/), (/diag_supernode, solve_supernode, solve_work/))

  end subroutine

  subroutine border_sym_update(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    type(c_ptr), pointer :: rect, diag
    integer :: nrow, ssize, wsize

    nrow = node_data%get_matrix_block_size(i, node)
    ssize = node_data%border_supernode_size(node)
    wsize = node_data%border_work_size(node)
    rect => factors%get_supernode(node, i, j)
    diag => factors%get_matrix(node, i, i)
    call sym_update_task%insert_task((/nrow, ssize/), (/rect, diag/))

  end subroutine

  subroutine border_update(node_data, factors, node, upper_idx, lower_idx, col_idx)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, upper_idx, lower_idx, col_idx
    type(c_ptr), pointer :: lower, upper, update
    integer :: upper_n, lower_n, ssize, wsize

    upper_n = node_data%get_matrix_block_size(upper_idx, node)
    lower_n = node_data%get_matrix_block_size(lower_idx, node)
    ssize = node_data%border_supernode_size(node)
    wsize = node_data%border_work_size(node)
    upper => factors%get_supernode(node, upper_idx, col_idx)
    lower => factors%get_supernode(node, lower_idx, col_idx)
    update => factors%get_matrix(node, lower_idx, upper_idx)
    call update_task%insert_task((/ssize, lower_n, upper_n/), (/lower, upper, update/))

  end subroutine
end module