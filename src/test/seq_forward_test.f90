program seq_forward_test
  use factors_m
  use right_hand_m
  use block_local_index_m
  use jagged_array_m
  use contiguous_sets_m
  use seq_forward_m
  use test_util
  use node_data_m
  implicit none
  type(factors_c), pointer :: factors
  type(block_local_index_c), pointer :: block_local_index
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(right_hand_c), pointer :: rh
  double precision, pointer, contiguous :: val(:), matrix(:), rh_val(:)
  integer :: nb, i

  nb=2
  node_sets => create_contiguous_sets([4,5,4])
  local_index => create_jagged_array([5,3,0],[2,3,4,5,7,1,3,4])
  node_data => create_node_data([4,5,4],[5,3,0],nb)
  factors => create_factors(node_data, nb)
  block_local_index => create_block_local_index(node_data, local_index)
  rh => create_right_hand(node_data, node_sets, local_index, nb)

  allocate(rh_val, source=[double precision::1,4,9,16,1,8,17,28,41,6,4,39,31])
  call rh%set_val(rh_val)
  
  factors%get_supernode_ptr(1,1,1) = [1,0,2,2]
  factors%get_supernode_ptr(1,2,1) = [3,3,4,4]
  factors%get_supernode_ptr(1,3,1) = [1,1,2,2]
  factors%get_supernode_ptr(1,4,1) = [3,3,4,4]
  factors%get_supernode_ptr(1,5,1) = [5,5]
  factors%get_supernode_ptr(1,2,2) = [3,0,4,4]
  factors%get_supernode_ptr(1,3,2) = [1,1,2,2]
  factors%get_supernode_ptr(1,4,2) = [3,3,4,4]
  factors%get_supernode_ptr(1,5,2) = [5,5]
  factors%get_supernode_ptr(2,1,1) = [1,0,2,2]
  factors%get_supernode_ptr(2,2,1) = [3,3,4,4]
  factors%get_supernode_ptr(2,3,1) = [5,5,1,1]
  factors%get_supernode_ptr(2,4,1) = [2,2,3,3]
  factors%get_supernode_ptr(2,2,2) = [3,0,4,4]
  factors%get_supernode_ptr(2,3,2) = [5,5,1,1]
  factors%get_supernode_ptr(2,4,2) = [2,2,3,3]
  factors%get_supernode_ptr(2,3,3) = [5,1]
  factors%get_supernode_ptr(2,4,3) = [2,3]
  factors%get_supernode_ptr(3,1,1) = [1,0,2,2]
  factors%get_supernode_ptr(3,2,1) = [3,3,4,4]
  factors%get_supernode_ptr(3,2,2) = [3,0,4,4]

  call seq_forward(factors, rh, block_local_index, [2,3,0])
  call assert_equal("nb=2", rh_val, [(1d0, i=1,13)])

  nb=3
  node_sets => create_contiguous_sets([4,5,4])
  local_index => create_jagged_array([5,3,0],[2,3,4,5,7,1,3,4])
  node_data => create_node_data([4,5,4],[5,3,0],nb)
  factors => create_factors(node_data, nb)
  block_local_index => create_block_local_index(node_data, local_index)
  rh => create_right_hand(node_data, node_sets, local_index, nb)

  allocate(rh_val, source=[double precision::1,4,9,16,1,8,17,28,41,6,4,39,31])
  call rh%set_val(rh_val)
  
  factors%get_supernode_ptr(1,1,1) = [1,0,0,2,2,0,3,3,3]
  factors%get_supernode_ptr(1,2,1) = [4,4,4,1,1,1,2,2,2]
  factors%get_supernode_ptr(1,3,1) = [3,3,3,4,4,4,5,5,5]
  factors%get_supernode_ptr(1,2,2) = [4,1,2]
  factors%get_supernode_ptr(1,3,2) = [3,4,5]
  factors%get_supernode_ptr(2,1,1) = [1,0,0,2,2,0,3,3,3]
  factors%get_supernode_ptr(2,2,1) = [4,4,4,5,5,5,1,1,1]
  factors%get_supernode_ptr(2,3,1) = [2,2,2,3,3,3]
  factors%get_supernode_ptr(2,2,2) = [4,0,5,5,1,1]
  factors%get_supernode_ptr(2,3,2) = [2,2,3,3]
  factors%get_supernode_ptr(3,1,1) = [1,0,0,2,2,0,3,3,3]
  factors%get_supernode_ptr(3,2,1) = [4,4,4,4]
  factors%get_supernode_ptr(3,2,2) = [4]

  call seq_forward(factors, rh, block_local_index, [2,3,0])
  call assert_equal("nb=3", rh_val, [(1d0, i=1,13)])

contains

end program seq_forward_test