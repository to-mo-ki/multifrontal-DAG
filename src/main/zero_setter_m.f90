module zero_setter_m
  use set_zero_kernel_m
  use factors_m
  implicit none
  private

  public :: set_zero
  
contains

  subroutine set_zero(factors)
    type(factors_c), pointer :: factors
    integer :: i, j, node, ncol, nrow
    double precision, pointer, contiguous :: a(:)
    do node=1, factors%get_num_node()
      do j=1, factors%get_num_block(node)
        ncol = factors%get_block_size(j, node)
        a => factors%get_matrix_ptr(node, j, j)
        call set_zero_tri(a, ncol)
        do i=j+1, factors%get_num_block(node)
          nrow = factors%get_block_size(i, node)
          a => factors%get_matrix_ptr(node, j, j)
          call set_zero_rect(a, ncol, nrow)
        enddo
      enddo
    enddo
  end subroutine

end module