module solve_subroutines_m
  use node_data_m
  use solve_kernel_m
  use factors_m
  use right_hand_m
  implicit none
  
contains
  subroutine forward(node_data, factors, rh, node, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, j
    integer :: n
    double precision, pointer, contiguous :: a(:), b(:)

    a => factors%get_supernode(node, j, j)
    b => rh%get_array_ptr(node, j)
    n = node_data%get_matrix_block_size(j, node)
    call mydtrsv_l(a, n, b)

  end subroutine

  subroutine update_l(node_data, factors, rh, node, i, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, i, j
    integer :: ncol, nrow
    double precision, pointer, contiguous :: a(:), b1(:), b2(:)
    
    a => factors%get_supernode(node, i, j)
    b1 => rh%get_array_ptr(node, j)
    b2 => rh%get_array_ptr(node, i)
    ncol = node_data%get_supernode_block_size(j, node)
    nrow = node_data%get_matrix_block_size(i, node)
    call mydgemv_t(a, ncol, nrow, b1, b2)

  end subroutine

  subroutine backward(node_data, factors, rh, node, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, j
    integer :: n
    double precision, pointer, contiguous :: a(:), b(:)

    a => factors%get_supernode(node, j, j)
    b => rh%get_array_ptr(node, j)
    n = node_data%get_matrix_block_size(j, node)
    call mydtrsv_u(a, n, b)

  end subroutine

  subroutine update_u(node_data, factors, rh, node, i, j)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, i, j
    integer :: ncol, nrow
    double precision, pointer, contiguous :: a(:), b1(:), b2(:)
    
    a => factors%get_supernode(node, i, j)
    b1 => rh%get_array_ptr(node, i)
    b2 => rh%get_array_ptr(node, j)
    ncol = node_data%get_supernode_block_size(j, node)
    nrow = node_data%get_matrix_block_size(i, node)
    call mydgemv_n(a, ncol, nrow, b1, b2)

  end subroutine

end module