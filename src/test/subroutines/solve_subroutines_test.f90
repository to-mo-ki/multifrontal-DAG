program sovle_subroutines_test
  ![4    ] [1]   [4]
  ![3 3  ] [1] = [6]
  ![2 2 2] [1]   [6]

  ![1 1 1] [1] = [3]
  ![2 2 2] [1] = [6]
  !        [1]

  ![9 ]   [1 1][1]  [9 ]   [3]=[6]
  ![12] - [2 2][2] =[12] - [6]=[6]
  ![13]   [2 2]     [13]   [9]=[4]

  ![2 2 2] [1]   [6]
  ![  3 3] [1] = [6]
  ![    4] [1]   [4]
  use solve_subroutines_m
  use factors_m
  use right_hand_m
  use node_data_m
  use contiguous_sets_m
  use jagged_array_m
  use test_util
  implicit none

  type(factors_c), pointer :: factors
  type(right_hand_c), pointer :: rh
  type(node_data_c), pointer :: node_data
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: ccs
  integer :: nb

  nb = 3
  node_data => create_node_data([3,2],[2,0],nb)
  node_sets => create_contiguous_sets([3,2])
  ccs => create_jagged_array([2,0])
  factors => create_factors(node_data, node_sets, ccs, nb)
  rh => create_right_hand(node_data, node_sets, ccs, nb)

  factors%get_supernode_ptr(1,1,1) = [4d0,0d0,0d0,3d0,3d0,0d0,2d0,2d0,2d0]
  factors%get_supernode_ptr(1,2,1) = [1d0,1d0,1d0,2d0,2d0,2d0]
  rh%get_array_ptr(1,1) = [4d0,6d0,6d0]
  rh%get_array_ptr(1,2) = [3d0,6d0]

  call forward(factors, rh, 1, 1)
  call assert_equal("forward", rh%get_array_ptr(1,1), [1d0,1d0,1d0])
  call update_l(factors, rh, 1, 2, 1)
  call assert_equal("update_l", rh%get_array_ptr(1,2), [0d0,0d0])

  factors%get_supernode_ptr(1,1,1) = [2d0,0d0,0d0,2d0,3d0,0d0,2d0,3d0,4d0]
  factors%get_supernode_ptr(1,2,1) = [1d0,2d0,3d0,1d0,2d0,3d0]
  
  rh%get_array_ptr(1,1) = [9d0,12d0,13d0]
  rh%get_array_ptr(1,2) = [1d0,2d0]

  call update_u(factors, rh, 1, 2, 1)
  call assert_equal("update_u", rh%get_array_ptr(1,1), [6d0,6d0,4d0])
  call backward(factors, rh, 1, 1)
  call assert_equal("backward", rh%get_array_ptr(1,1), [1d0,1d0,1d0])

end program sovle_subroutines_test