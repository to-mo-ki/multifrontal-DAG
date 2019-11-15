program block_local_index_creator_test
  use contiguous_sets_m
  use jagged_array_m
  use block_local_index_creator_m
  use node_data_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(jagged_array_c), pointer :: parent_block_num, parent_block_ptr
  type(contiguous_sets_c), pointer :: parent_ptr
  
  local_index => create_jagged_array((/4, 0/), (/2, 3, 7, 8/))
  node_data => create_node_data((/5, 8/), (/8, 0/), 3)

  parent_ptr => create_parent_ptr(node_data, local_index)
  call assert_equal("parent_ptr", parent_ptr%get_length(1), 2)

  parent_block_num => create_parent_block_num(node_data, local_index, parent_ptr)
  call assert_equal("parent_block_num", parent_block_num%get_array(1), (/1, 3/))

  parent_block_ptr => create_parent_block_ptr(node_data, local_index, parent_ptr)
  call assert_equal("parent_block_num", parent_block_num%get_array(1), (/1, 3/))


  

end program block_local_index_creator_test