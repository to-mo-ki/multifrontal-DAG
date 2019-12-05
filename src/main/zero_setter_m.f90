module zero_setter_m
  use node_data_m
  use set_zero_kernel_m
  use factors_m
  implicit none
  private

  public :: set_zero
  
contains

  subroutine set_zero(node_data, factors)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer :: i, j, node, ncol, nrow
    double precision, pointer, contiguous :: a(:)
    do node=1, node_data%num_node
      do j=1, node_data%get_num_matrix_block(node)
        ncol = node_data%get_matrix_block_size(j, node)
        a => factors%get_matrix_ptr(node, j, j)
        call set_zero_tri(a, ncol)
        do i=j+1, node_data%get_num_matrix_block(node)
          nrow = node_data%get_matrix_block_size(i, node)
          a => factors%get_matrix_ptr(node, j, j)
          call set_zero_rect(a, ncol, nrow)
        enddo
      enddo
    enddo
  end subroutine

end module