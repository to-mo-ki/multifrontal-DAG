program generate_local_index_test
  use jagged_array_m
  use contiguous_sets_m
  use generate_local_index_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: row_index, local_index, tree_child
  type(contiguous_sets_c), pointer :: node_sets
  integer, pointer, contiguous :: num_child(:), child_val(:)
  integer, pointer, contiguous :: row(:), num_row(:)

  allocate(num_child(7), child_val(6))
  num_child = (/0, 1, 0, 0, 1, 2, 2/)
  child_val = (/1, 4, 3, 5, 2, 6/)
  tree_child => create_jagged_array(num_child, child_val)

  allocate(num_row(7), row(12))
  num_row = (/2, 2, 2, 2, 2, 2, 0/)
  row = (/2, 8, 8, 9, 7, 9, 6, 8, 7, 8, 8, 9/)
  row_index => create_jagged_array(num_row, row)

  node_sets => create_contiguous_sets((/1, 1, 2, 1, 1, 1, 2/))

  local_index => generate_local_index(row_index, node_sets, tree_child)

  call assert_equal("1th local index", local_index%get_array(1), (/1, 2/))
  call assert_equal("2th local index", local_index%get_array(2), (/1, 2/))
  call assert_equal("3th local index", local_index%get_array(3), (/1, 3/))
  call assert_equal("4th local index", local_index%get_array(4), (/1, 3/))
  call assert_equal("5th local index", local_index%get_array(5), (/1, 2/))
  call assert_equal("6th local index", local_index%get_array(6), (/1, 2/))
  call assert_equal("7th local index", local_index%get_array_length(7), 0)
  

  
end program generate_local_index_test