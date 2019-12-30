program seq_forward_test
  use factors_m
  use right_hand_m
  use jagged_array_3D_m
  use block_local_index_info_m
  use jagged_array_m
  use contiguous_sets_m
  use seq_forward_m
  use test_util
  use node_data_m
  implicit none
  type(factors_c), pointer :: factors
  type(jagged_array_3D_c), pointer :: block_local_index
  type(block_local_index_info_c), pointer :: block_local_index_info
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(right_hand_c), pointer :: rh
  double precision, pointer, contiguous :: rh_val(:), ptr(:)
  integer :: nb, i

  nb=2
  node_sets => create_contiguous_sets([4,5,4])
  local_index => create_jagged_array([5,3,0],[2,3,4,5,7,1,3,4])
  node_data => create_node_data([4,5,4],[5,3,0],nb)
  factors => create_factors(node_data)
  block_local_index_info => create_block_local_index_info(node_data, local_index)
  block_local_index => block_local_index_info%create_block_local_index()
  rh => create_right_hand(node_data, nb)

  allocate(rh_val(13))
  rh_val = [1,4,9,16,1,8,17,28,41,6,4,39,31]
  call rh%set_val(rh_val)
  
  ptr => factors%get_supernode(1,1,1); ptr = [1,0,2,2]
  ptr => factors%get_supernode(1,2,1); ptr = [3,3,4,4]
  ptr => factors%get_supernode(1,3,1); ptr = [1,1,2,2]
  ptr => factors%get_supernode(1,4,1); ptr = [3,3,4,4]
  ptr => factors%get_supernode(1,5,1); ptr = [5,5]
  ptr => factors%get_supernode(1,2,2); ptr = [3,0,4,4]
  ptr => factors%get_supernode(1,3,2); ptr = [1,1,2,2]
  ptr => factors%get_supernode(1,4,2); ptr = [3,3,4,4]
  ptr => factors%get_supernode(1,5,2); ptr = [5,5]
  ptr => factors%get_supernode(2,1,1); ptr = [1,0,2,2]
  ptr => factors%get_supernode(2,2,1); ptr = [3,3,4,4]
  ptr => factors%get_supernode(2,3,1); ptr = [5,5,1,1]
  ptr => factors%get_supernode(2,4,1); ptr = [2,2,3,3]
  ptr => factors%get_supernode(2,2,2); ptr = [3,0,4,4]
  ptr => factors%get_supernode(2,3,2); ptr = [5,5,1,1]
  ptr => factors%get_supernode(2,4,2); ptr = [2,2,3,3]
  ptr => factors%get_supernode(2,3,3); ptr = [5,1]
  ptr => factors%get_supernode(2,4,3); ptr = [2,3]
  ptr => factors%get_supernode(3,1,1); ptr = [1,0,2,2]
  ptr => factors%get_supernode(3,2,1); ptr = [3,3,4,4]
  ptr => factors%get_supernode(3,2,2); ptr = [3,0,4,4]

  call seq_forward(node_data, factors, rh, block_local_index, block_local_index_info, [2,3,0])
  call assert_equal("nb=2", rh_val, [(1d0, i=1,13)])

  nb=3
  node_sets => create_contiguous_sets([4,5,4])
  local_index => create_jagged_array([5,3,0],[2,3,4,5,7,1,3,4])
  node_data => create_node_data([4,5,4],[5,3,0],nb)
  factors => create_factors(node_data)
  block_local_index_info => create_block_local_index_info(node_data, local_index)
  block_local_index => block_local_index_info%create_block_local_index()
  rh => create_right_hand(node_data, nb)

  allocate(rh_val(13))
  rh_val = [1,4,9,16,1,8,17,28,41,6,4,39,31]
  call rh%set_val(rh_val)
  
  ptr => factors%get_supernode(1,1,1); ptr = [1,0,0,2,2,0,3,3,3]
  ptr => factors%get_supernode(1,2,1); ptr = [4,4,4,1,1,1,2,2,2]
  ptr => factors%get_supernode(1,3,1); ptr = [3,3,3,4,4,4,5,5,5]
  ptr => factors%get_supernode(1,2,2); ptr = [4,1,2]
  ptr => factors%get_supernode(1,3,2); ptr = [3,4,5]
  ptr => factors%get_supernode(2,1,1); ptr = [1,0,0,2,2,0,3,3,3]
  ptr => factors%get_supernode(2,2,1); ptr = [4,4,4,5,5,5,1,1,1]
  ptr => factors%get_supernode(2,3,1); ptr = [2,2,2,3,3,3]
  ptr => factors%get_supernode(2,2,2); ptr = [4,0,5,5,1,1]
  ptr => factors%get_supernode(2,3,2); ptr = [2,2,3,3]
  ptr => factors%get_supernode(3,1,1); ptr = [1,0,0,2,2,0,3,3,3]
  ptr => factors%get_supernode(3,2,1); ptr = [4,4,4,4]
  ptr => factors%get_supernode(3,2,2); ptr = [4]

  call seq_forward(node_data, factors, rh, block_local_index, block_local_index_info, [2,3,0])
  call assert_equal("nb=3", rh_val, [(1d0, i=1,13)])

end program seq_forward_test