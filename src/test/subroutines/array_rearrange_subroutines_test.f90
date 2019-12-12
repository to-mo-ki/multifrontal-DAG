program array_rearrange_subroutines_test
  use array_rearrange_subroutines_m
  use node_data_m
  use right_hand_m
  use test_util
  implicit none
  type(node_data_c), pointer :: node_data
  type(right_hand_c), pointer :: rh

  node_data => create_node_data([8,9],[9,0], 5)
  rh => create_right_hand(node_data, 5)
  rh%get_supernode(1, 2) = 1d0
  rh%get_work(1, 2) = 1d0
  rh%get_array_ptr(1, 2) = [1d0,2d0,3d0,4d0,5d0]

  call rearrange_array_f(rh, 1, 2)
  call assert_equal("left", rh%get_supernode(1, 2), [2d0,3d0,4d0])
  call assert_equal("right", rh%get_work(1, 2), [4d0,5d0])

  rh%get_supernode(1, 2) = [1,2,3]
  rh%get_work(1, 2) = [4,5]
  rh%get_array_ptr(1, 2) = 0

  call rearrange_array_b(rh, 1, 2)
  call assert_equal("dest", rh%get_array_ptr(1, 2), [1d0,2d0,3d0,4d0,5d0])
  
end program 