program symbolic_factorize_test
  use jagged_array_m
  use contiguous_sets_m
  use symbolic_factorize_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs_a, ccs_l, tree_child
  integer, pointer, contiguous :: num_row(:), row(:), cc(:), num_child(:), child_val(:)
  type(contiguous_sets_c), pointer :: node_sets

  allocate(num_row(7), row(10))
  num_row = (/2, 1, 2, 2, 2, 1, 0/)
  row = (/2, 8, 9, 7, 9, 6, 8, 7, 8, 9/)
  ccs_a => create_jagged_array(num_row, row)
  node_sets => create_contiguous_sets((/1, 1, 2, 1, 1, 1, 2/))

  call make_supernodal_ccs(cc)

  call make_supernodal_tree(num_child, child_val)
  tree_child => create_jagged_array(num_child, child_val)

  ccs_l => symbolic_factorize(ccs_a, node_sets, cc, tree_child)
  

  call assert_equal("col", ccs_l%get_val(), (/2, 8, 8, 9, 7, 9, 6, 8, 7, 8, 8, 9/))
  
end program symbolic_factorize_test