program fundamental_supernode_test
  use jagged_array_m
  use fundamental_supernode_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs, tree_child, ccs_supernode
  integer, pointer, contiguous :: col(:), row(:), child_ptr(:), child_val(:)
  integer, pointer, contiguous :: isleaf(:), first_node(:), num_child_supernode(:)
  integer, pointer, contiguous :: cc_node(:), cc_supernode(:)

  call make_ccs_postordering(col, row)
  ccs = create_jagged_array(col, row)
  allocate(child_ptr(10))
  allocate(child_val(9))
  child_ptr = (/1, 1, 2, 2, 3, 3, 4, 6, 8, 9/)
  child_val = (/1, 3, 5, 4, 6, 2, 7, 8, 0/)
  tree_child = create_jagged_array(child_ptr, child_val)
  allocate(isleaf(9))
  isleaf = (/1, 1, 1, 0, 1, 1, 0, 0, 0/)

  first_node => search_first_node_in_supernode(isleaf, tree_child)
  call assert_equal("first node", first_node, (/1, 2, 3, 5, 6, 7, 8, 10/))


  num_child_supernode => create_supernodal_tree(first_node, tree_child)
  call assert_equal("supernodal tree", num_child_supernode, (/0, 1, 0, 0, 1, 2, 2/))

  ccs_supernode = create_supernodal_ccs(first_node, ccs)
  
  call assert_equal("supernodal ccs(1)", ccs_supernode%get_array(1), (/2, 8/))
  call assert_equal("supernodal ccs(2)", ccs_supernode%get_array(2), (/9/))
  call assert_equal("supernodal ccs(3)", ccs_supernode%get_array(3), (/7, 9/))
  call assert_equal("supernodal ccs(4)", ccs_supernode%get_array(4), (/6, 8/))
  call assert_equal("supernodal ccs(5)", ccs_supernode%get_array(5), (/7, 8/))
  call assert_equal("supernodal ccs(6)", ccs_supernode%get_array(6), (/9/))

  allocate(cc_node(9))
  cc_node = (/3, 3, 4, 3, 3, 3, 3, 2, 1/)
  cc_supernode => create_supernodal_column_count(first_node, cc_node)
  call assert_equal("column count in supernode", cc_supernode, (/2, 2, 2, 2, 2, 2, 0/))

  
end program fundamental_supernode_test