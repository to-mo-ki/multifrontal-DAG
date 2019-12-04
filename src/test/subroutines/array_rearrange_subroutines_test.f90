program array_rearrange_subroutines_test
  use array_rearrange_subroutines_m
  use contiguous_sets_m
  use node_data_m
  use jagged_array_m
  use right_hand_m
  use test_util
  implicit none
  type(contiguous_sets_c), pointer :: node_sets
  type(node_data_c), pointer :: node_data
  type(jagged_array_c), pointer :: ccs
  type(right_hand_c), pointer :: rh

  node_sets => create_contiguous_sets([8,9])
  ccs => create_jagged_array([9,0])
  node_data => create_node_data([8,9],[9,0], 5)
  rh => create_right_hand(node_data, node_sets, ccs, 5)
  
  rh%get_array_ptr(1, 2) = [1d0,2d0,3d0,4d0,5d0]
  call rearrange_array(rh, 1, 2)
  call assert_equal("left", rh%get_supernode_ptr(1, 2), [1d0,2d0,3d0])
  call assert_equal("right", rh%get_work_ptr(1, 2), [4d0,5d0])
  
end program 