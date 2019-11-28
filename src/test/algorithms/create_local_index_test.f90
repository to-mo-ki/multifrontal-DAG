program create_local_index_test
  use create_local_index_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  type(jagged_array_c), pointer :: ccs, local_index, tree_child
  type(contiguous_sets_c), pointer :: node_sets
  integer, pointer, contiguous :: num_row(:), row(:), node_size(:)
  integer, pointer, contiguous :: num_child(:), child_val(:)

  call make_supernodal_ccs(num_row, row, node_size)
  ccs => create_jagged_array(num_row, row)
  node_sets => create_contiguous_sets(node_size)
  call make_supernodal_tree(num_child, child_val)
  tree_child => create_jagged_array(num_child, child_val)

  local_index => create_local_index(ccs, node_sets, tree_child)
  call assert_equal("local index", local_index%get_val(), [1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2])


  
end program create_local_index_test