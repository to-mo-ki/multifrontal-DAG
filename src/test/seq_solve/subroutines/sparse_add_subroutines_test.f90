program sparse_add_subroutines_test
  use factors_m
  use sparse_add_subroutines_m
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use node_data_m
  use block_local_index_info_m
  use right_hand_m
  use test_util
  implicit none
  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_set
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(jagged_array_3D_c), pointer :: block_local_index
  type(block_local_index_info_c), pointer :: block_local_index_info
  type(right_hand_c), pointer :: rh
  double precision, pointer, contiguous :: ptr(:)

  local_index => create_jagged_array([7, 3, 0], [3,4,5,6,8,9,10,1,2,3])
  node_set => create_contiguous_sets([4, 7, 3])
  node_data => create_node_data([4,7,3],[7,3,0],3)
  factors => create_factors(node_data)
  node_data => create_node_data([4,7,3], [7,3,0], 3)
  block_local_index_info => create_block_local_index_info(node_data, local_index)
  block_local_index => block_local_index_info%create_block_local_index()
  rh => create_right_hand(node_data, 3)
  
  ptr => rh%get_work(1,2); ptr = [1,2]
  ptr => rh%get_work(1,3); ptr = [3,4,5]
  ptr => rh%get_work(1,4); ptr = [6,7]
  ptr => rh%get_array_ptr(2,1); ptr = 0d0
  ptr => rh%get_array_ptr(2,2); ptr = 0d0
  ptr => rh%get_array_ptr(2,3); ptr = 0d0
  ptr => rh%get_array_ptr(2,4); ptr = 0d0
  
  call start_array_tests("scatter_add")
  call scatter_add(rh, block_local_index, block_local_index_info, 1, 1, 2)
  call add_test("node=1, index=1", rh%get_array_ptr(2, 1), [0d0,0d0,1d0])
  call scatter_add(rh, block_local_index, block_local_index_info, 2, 1, 2)
  call add_test("node=1, index=2", rh%get_array_ptr(2, 2), [2d0,0d0,0d0])
  call scatter_add(rh, block_local_index, block_local_index_info, 3, 1, 2)
  call add_test("node=1, index=3", rh%get_array_ptr(2, 2), [2d0,3d0,4d0])
  call scatter_add(rh, block_local_index, block_local_index_info, 4, 1, 2)
  call add_test("node=1, index=4", rh%get_array_ptr(2, 3), [0d0,5d0,0d0])
  call scatter_add(rh, block_local_index, block_local_index_info, 5, 1, 2)
  call add_test("node=1, index=5", rh%get_array_ptr(2, 3), [0d0,5d0,6d0])
  call scatter_add(rh, block_local_index, block_local_index_info, 6, 1, 2)
  call add_test("node=1, index=6", rh%get_array_ptr(2, 4), [7d0])
  call end_array_tests()
  
  ptr => rh%get_work(1,2); ptr = 0d0
  ptr => rh%get_work(1,3); ptr = 0d0
  ptr => rh%get_work(1,4); ptr = 0d0

  ptr => rh%get_array_ptr(2,1); ptr = [1,2,3]
  ptr => rh%get_array_ptr(2,2); ptr = [4,5,6]
  ptr => rh%get_array_ptr(2,3); ptr = [7,8,9]
  ptr => rh%get_array_ptr(2,4); ptr = [10]

  call start_array_tests("gather")
  call gather(rh, block_local_index, block_local_index_info, 1, 1, 2)
  call add_test("node=1, index=1", rh%get_work(1, 2), [3d0,0d0])
  call gather(rh, block_local_index, block_local_index_info, 2, 1, 2)
  call add_test("node=1, index=2", rh%get_work(1, 2), [3d0,4d0])
  call gather(rh, block_local_index, block_local_index_info, 3, 1, 2)
  call add_test("node=1, index=3", rh%get_array_ptr(1, 3), [5d0,6d0,0d0])
  call gather(rh, block_local_index, block_local_index_info, 4, 1, 2)
  call add_test("node=1, index=4", rh%get_array_ptr(1, 3), [5d0,6d0,8d0])
  call gather(rh, block_local_index, block_local_index_info, 5, 1, 2)
  call add_test("node=1, index=5", rh%get_array_ptr(1, 4), [9d0,0d0])
  call gather(rh, block_local_index, block_local_index_info, 6, 1, 2)
  call add_test("node=1, index=6", rh%get_array_ptr(1, 4), [9d0,10d0])
  call end_array_tests()
  
end program