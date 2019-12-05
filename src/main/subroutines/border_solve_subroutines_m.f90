module border_solve_subroutines_m
  use border_solve_kernel_m
  use solve_kernel_m
  use factors_m
  use right_hand_m
  implicit none
  private

  public :: border_forward, border_update_l, border_backward, border_update_u

contains
  subroutine border_forward(factors, rh, node, j)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: a(:), b1(:), b2(:)
    integer :: ncol, nrow

    ncol = factors%get_supernode_block_size(j, node)
    nrow = factors%get_block_size(j, node)
    a => factors%get_supernode_ptr(node, j, j)
    b1 => rh%get_supernode_ptr(node, j)
    b2 => rh%get_work_ptr(node, j)
    call border_dtrsv_l(a, b1, b2, ncol, nrow-ncol)
    
  end subroutine

  subroutine border_update_l(factors, rh, node, i, j)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: a(:), b1(:), b2(:)
    integer :: ncol, nrow

    a => factors%get_supernode_ptr(node, i, j)
    b1 => rh%get_supernode_ptr(node, j)
    b2 => rh%get_array_ptr(node, i)
    ncol = factors%get_supernode_block_size(j, node)
    nrow = factors%get_block_size(i, node)
    call mydgemv_t(a, ncol, nrow, b1, b2)

  end subroutine

  subroutine border_backward(factors, rh, node, j)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, j
    double precision, pointer, contiguous :: a(:), b1(:), b2(:)
    integer :: ncol, nrow

    ncol = factors%get_supernode_block_size(j, node)
    nrow = factors%get_block_size(j, node)
    a => factors%get_supernode_ptr(node, j, j)
    b1 => rh%get_supernode_ptr(node, j)
    b2 => rh%get_work_ptr(node, j)
    call border_dtrsv_u(a, b1, b2, ncol, nrow-ncol)
    
  end subroutine

  subroutine border_update_u(factors, rh, node, i, j)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, i, j
    integer :: ncol, nrow
    double precision, pointer, contiguous :: a(:), b1(:), b2(:)
    
    a => factors%get_supernode_ptr(node, i, j)
    b1 => rh%get_array_ptr(node, i)
    b2 => rh%get_supernode_ptr(node, j)
    ncol = factors%get_supernode_block_size(j, node)
    nrow = factors%get_block_size(i, node)
    call mydgemv_n(a, ncol, nrow, b1, b2)

  end subroutine

end module