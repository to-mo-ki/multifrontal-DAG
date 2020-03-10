program cost_calculator_test
  use node_data_m
  use cost_calculator_m
  use test_util
  implicit none
  type(node_data_c), pointer :: node_data
  integer :: cost, nonzero
  integer(8) :: big_cost

  node_data => create_node_data([5, 8], [4,0], 3)
  cost = calculate_cost(node_data)
  call assert_equal("test1", cost, 459)

  node_data => create_node_data([1], [999999999], 3)
  big_cost = calculate_cost(node_data)
  call assert_equal("big test", big_cost, 1000000000000000000)

  node_data => create_node_data([5, 8], [4,0], 3)
  nonzero = count_nonzero(node_data)
  call assert_equal("count nonzero", nonzero, 71)
  
end program