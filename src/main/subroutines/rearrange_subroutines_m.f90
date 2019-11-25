module rearrange_subroutines_m
  use factors_m
  use matrix_splitter_m
  implicit none
  private

  public :: rearrange_diag, rearrange_ndiag

contains

  subroutine rearrange_diag(factors, node, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: matrix(:), supernode(:), work(:)
    integer :: ssize, wsize

    matrix => factors%get_matrix_ptr(node, j, j)
    supernode => factors%get_supernode_ptr(node, j, j)
    work => factors%get_work_ptr(node, j, j)
    call factors%get_border_info(node, ssize, wsize)
    call split_tri_matrix(matrix, supernode, work, ssize, wsize, ssize+wsize)
    
  end subroutine

  subroutine rearrange_ndiag(factors, node, i, j)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: matrix(:), supernode(:), work(:)
    integer :: ssize, wsize, nrow

    matrix => factors%get_matrix_ptr(node, i, j)
    supernode => factors%get_supernode_ptr(node, i, j)
    work => factors%get_work_ptr(node, i, j)
    call factors%get_border_info(node, ssize, wsize)
    nrow = factors%get_block_size(i, node)
    call split_rect_matrix(matrix, supernode, work, ssize, wsize, nrow, ssize+wsize)

  end subroutine

end module