module cost_calculator_m
  use node_data_m
  use integer_function_m
  implicit none
  private

  public :: calculate_cost
contains
  integer(8) function calculate_cost(node_data) result(cost)
    type(node_data_c), pointer :: node_data
    integer :: node, col, row

    cost = 0
    do node=1, node_data%num_node
      col = node_data%supernode_size(node)
      row = node_data%work_size(node)
      cost = cost + partial_square_sum(row+1, row+col)
    enddo

  end function

end module