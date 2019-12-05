module coefficient_setter_m
  use node_data_m
  use factors_m
  use ccs_m
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  private

  public :: set_coefficient

contains

  subroutine set_coefficient(node_data, ccs, node_sets, factors, nb)
    type(node_data_c), pointer :: node_data
    type(ccs_c), pointer :: ccs
    type(contiguous_sets_c), pointer :: node_sets
    type(factors_c), pointer :: factors
    integer :: node, i, j, block_col_num, block_row_num, nb, block_i, block_j, ld, col_num
    integer, pointer, contiguous :: rows(:)
    double precision, pointer, contiguous :: vals(:), block_matrix(:)

    do node=1, node_data%num_node
      do j=node_sets%get_first(node), node_sets%get_last(node)
        rows => ccs%get_row_array(j)
        vals => ccs%get_val_array(j)
        col_num = j - node_sets%get_first(node) + 1
        block_col_num = (col_num-1)/nb + 1
        block_j = mod(col_num-1, nb)+1
        do i=1, size(rows)
          block_row_num = (rows(i)-1)/nb + 1
          block_matrix => factors%get_matrix_ptr(node, block_row_num, block_col_num)
          block_i = mod(rows(i)-1, nb)+1
          ld = node_data%get_matrix_block_size(block_col_num, node)
          call set_cofficent_element(block_matrix, block_i, block_j, vals(i), ld)
        enddo
      enddo
    enddo

  end subroutine

  subroutine set_cofficent_element(matrix, i, j, val, ld)
    double precision :: matrix(ld, *), val
    integer, intent(in) :: i, j, ld

    matrix(j, i) = val
  end subroutine

  

end module