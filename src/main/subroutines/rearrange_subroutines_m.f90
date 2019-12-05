module rearrange_subroutines_m
  use node_data_m
  use factors_m
  use matrix_splitter_m
  implicit none
  private

  public :: rearrange_diag, rearrange_ndiag

contains

  subroutine rearrange_diag(node_data, factors, node, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: matrix(:), supernode(:), work(:)
    integer :: ssize, wsize

    matrix => factors%get_matrix_ptr(node, j, j)
    supernode => factors%get_supernode_ptr(node, j, j)
    work => factors%get_work_ptr(node, j, j)
    ssize = node_data%get_border_supernode_size(node)
    wsize = node_data%get_border_work_size(node)
    call split_tri_matrix(matrix, supernode, work, ssize, wsize)
    
  end subroutine

  subroutine rearrange_ndiag(node_data, factors, node, i, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: matrix(:), supernode(:), work(:)
    integer :: ssize, wsize, nrow

    matrix => factors%get_matrix_ptr(node, i, j)
    supernode => factors%get_supernode_ptr(node, i, j)
    work => factors%get_work_ptr(node, i, j)
    ssize = node_data%get_border_supernode_size(node)
    wsize = node_data%get_border_work_size(node)
    nrow = node_data%get_block_size(i, node)
    call split_rect_matrix(matrix, supernode, work, ssize, wsize, nrow)

  end subroutine

end module