module starpu_rearrange_subroutines_m
  use node_data_m
  use starpu_factors_m
  use rearrange_tasks_m
  use iso_c_binding
  implicit none
  private

  public :: rearrange_diag, rearrange_ndiag

contains

  subroutine rearrange_diag(node_data, factors, node, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, j
    type(c_ptr), pointer :: matrix, supernode, work
    integer :: ssize, wsize

    matrix => factors%get_matrix(node, j, j)
    supernode => factors%get_supernode(node, j, j)
    work => factors%get_work(node, j, j)
    ssize = node_data%border_supernode_size(node)
    wsize = node_data%border_work_size(node)
    call split_tri_matrix_task%insert_task((/ssize, wsize/), (/matrix, supernode, work/))
    
  end subroutine

  subroutine rearrange_ndiag(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    type(c_ptr), pointer :: matrix, supernode, work
    integer :: ssize, wsize, nrow

    matrix => factors%get_matrix(node, i, j)
    supernode => factors%get_supernode(node, i, j)
    work => factors%get_work(node, i, j)
    ssize = node_data%border_supernode_size(node)
    wsize = node_data%border_work_size(node)
    nrow = node_data%get_matrix_block_size(i, node)
    call split_rect_matrix_task%insert_task((/ssize, wsize, nrow/), (/matrix, supernode, work/))

  end subroutine

end module