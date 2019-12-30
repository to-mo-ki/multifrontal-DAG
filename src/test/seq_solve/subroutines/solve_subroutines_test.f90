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
  use test_util
  implicit none

  type(factors_c), pointer :: factors
  type(right_hand_c), pointer :: rh
  type(node_data_c), pointer :: node_data
  integer :: nb
  double precision, pointer, contiguous :: ptr(:)

  nb = 3
  node_data => create_node_data([3,2],[2,0],nb)
  factors => create_factors(node_data)
  rh => create_right_hand(node_data, nb)

  ptr => factors%get_supernode(1,1,1); ptr = [4d0,0d0,0d0,3d0,3d0,0d0,2d0,2d0,2d0]
  ptr => factors%get_supernode(1,2,1); ptr = [1d0,1d0,1d0,2d0,2d0,2d0]
  ptr => rh%get_array_ptr(1,1); ptr = [4d0,6d0,6d0]
  ptr => rh%get_array_ptr(1,2); ptr = [3d0,6d0]

  call forward(node_data, factors, rh, 1, 1)
  call assert_equal("forward", rh%get_array_ptr(1,1), [1d0,1d0,1d0])
  call update_l(node_data, factors, rh, 1, 2, 1)
  call assert_equal("update_l", rh%get_array_ptr(1,2), [0d0,0d0])

  ptr => factors%get_supernode(1,1,1); ptr = [2d0,0d0,0d0,2d0,3d0,0d0,2d0,3d0,4d0]
  ptr => factors%get_supernode(1,2,1); ptr = [1d0,2d0,3d0,1d0,2d0,3d0]
  
  ptr => factors%get_supernode(1,1,1); ptr = [2d0,0d0,0d0,2d0,3d0,0d0,2d0,3d0,4d0]
  ptr => factors%get_supernode(1,2,1); ptr = [1d0,2d0,3d0,1d0,2d0,3d0]
  ptr => rh%get_array_ptr(1,1); ptr = [9d0,12d0,13d0]
  ptr => rh%get_array_ptr(1,2); ptr = [1d0,2d0]

  call update_u(node_data, factors, rh, 1, 2, 1)
  call assert_equal("update_u", rh%get_array_ptr(1,1), [6d0,6d0,4d0])
  call backward(node_data, factors, rh, 1, 1)
  call assert_equal("backward", rh%get_array_ptr(1,1), [1d0,1d0,1d0])

end program sovle_subroutines_test