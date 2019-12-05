program sparse_add_subroutines_test
  use factors_m
  use sparse_add_subroutines_m
  use contiguous_sets_m
  use jagged_array_m
  use node_data_m
  use block_local_index_m
  use right_hand_m
  use test_util
  implicit none
  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_set
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(block_local_index_c), pointer :: block_local_index
  type(right_hand_c), pointer :: rh
  double precision, pointer, contiguous :: a(:), val(:)

  local_index => create_jagged_array([7, 3, 0], [3,4,5,6,8,9,10,1,2,3])
  node_set => create_contiguous_sets([4, 7, 3])
  node_data => create_node_data([4,7,3],[7,3,0],3)
  factors => create_factors(node_data, 3)
  node_data => create_node_data([4,7,3], [7,3,0], 3)
  block_local_index => create_block_local_index(node_data, local_index)
  rh => create_right_hand(node_data, node_set, local_index, 3)
  
  rh%get_work_ptr(1,2) = [1,2]
  rh%get_work_ptr(1,3) = [3,4,5]
  rh%get_work_ptr(1,4) = [6,7]
  rh%get_array_ptr(2,1) = 0d0
  rh%get_array_ptr(2,2) = 0d0
  rh%get_array_ptr(2,3) = 0d0
  rh%get_array_ptr(2,4) = 0d0
  
  call start_array_tests("scatter_add")
  call scatter_add(rh, block_local_index, 1, 1, 2)
  call add_test("node=1, index=1", rh%get_array_ptr(2, 1), [0d0,0d0,1d0])
  call scatter_add(rh, block_local_index, 2, 1, 2)
  call add_test("node=1, index=2", rh%get_array_ptr(2, 2), [2d0,0d0,0d0])
  call scatter_add(rh, block_local_index, 3, 1, 2)
  call add_test("node=1, index=3", rh%get_array_ptr(2, 2), [2d0,3d0,4d0])
  call scatter_add(rh, block_local_index, 4, 1, 2)
  call add_test("node=1, index=4", rh%get_array_ptr(2, 3), [0d0,5d0,0d0])
  call scatter_add(rh, block_local_index, 5, 1, 2)
  call add_test("node=1, index=5", rh%get_array_ptr(2, 3), [0d0,5d0,6d0])
  call scatter_add(rh, block_local_index, 6, 1, 2)
  call add_test("node=1, index=6", rh%get_array_ptr(2, 4), [7d0])
  call end_array_tests()
  
  rh%get_work_ptr(1,2) = 0d0
  rh%get_work_ptr(1,3) = 0d0
  rh%get_work_ptr(1,4) = 0d0

  rh%get_array_ptr(2,1) = [1,2,3]
  rh%get_array_ptr(2,2) = [4,5,6]
  rh%get_array_ptr(2,3) = [7,8,9]
  rh%get_array_ptr(2,4) = [10]

  call start_array_tests("gather_add")
  call gather_add(rh, block_local_index, 1, 1, 2)
  call add_test("node=1, index=1", rh%get_work_ptr(1, 2), [3d0,0d0])
  call gather_add(rh, block_local_index, 2, 1, 2)
  call add_test("node=1, index=2", rh%get_work_ptr(1, 2), [3d0,4d0])
  call gather_add(rh, block_local_index, 3, 1, 2)
  call add_test("node=1, index=3", rh%get_array_ptr(1, 3), [5d0,6d0,0d0])
  call gather_add(rh, block_local_index, 4, 1, 2)
  call add_test("node=1, index=4", rh%get_array_ptr(1, 3), [5d0,6d0,8d0])
  call gather_add(rh, block_local_index, 5, 1, 2)
  call add_test("node=1, index=5", rh%get_array_ptr(1, 4), [9d0,0d0])
  call gather_add(rh, block_local_index, 6, 1, 2)
  call add_test("node=1, index=6", rh%get_array_ptr(1, 4), [9d0,10d0])
  call end_array_tests()
  
end program