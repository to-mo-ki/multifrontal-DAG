program border_solve_subroutines_test
  use border_solve_subroutines_m
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

  nb = 5
  node_data => create_node_data([3,6],[6,0],nb)
  node_sets => create_contiguous_sets([3,6])
  ccs => create_jagged_array([6,0])
  factors => create_factors(node_data, nb)
  rh => create_right_hand(node_data, node_sets, ccs, nb)

  factors%get_supernode_ptr(1,1,1) = [4d0,0d0,0d0,3d0,3d0,0d0,2d0,2d0,2d0,1d0,1d0,1d0,2d0,2d0,2d0]
  rh%get_supernode_ptr(1,1) = [4d0,6d0,6d0]
  rh%get_work_ptr(1,1) = [3d0,6d0]
  factors%get_supernode_ptr(1,2,1) = [1,1,1,2,2,2,3,3,3,4,4,4]
  rh%get_work_ptr(1,2) = [3,6,9,12]
  call border_forward(factors, rh, 1, 1)
  call assert_equal("border_forward", [rh%get_supernode_ptr(1,1), rh%get_work_ptr(1,1)], [1d0,1d0,1d0,0d0,0d0])
  call border_update_l(factors, rh, 1, 2, 1)
  call assert_equal("border_update_l", rh%get_work_ptr(2,1), [0d0,0d0,0d0,0d0,0d0])

  factors%get_supernode_ptr(1,1,1) = [2d0,0d0,0d0,2d0,3d0,0d0,2d0,3d0,4d0,1d0,2d0,3d0,1d0,2d0,3d0]
  factors%get_supernode_ptr(1,2,1) = [1,2,3,1,2,3,1,2,3,1,2,3]
  rh%get_supernode_ptr(1,1) = [9d0+10d0,12d0+20d0,13d0+30d0]
  rh%get_work_ptr(1,1) = [1d0,2d0]
  rh%get_work_ptr(1,2) = [1,2,3,4]

  call border_update_u(factors, rh, 1, 2, 1)
  call assert_equal("border_update_u", rh%get_supernode_ptr(1,1), [9d0,12d0,13d0])
  call border_backward(factors, rh, 1, 1)
  call assert_equal("border_backward", rh%get_supernode_ptr(1,1), [1d0,1d0,1d0])
  
end program