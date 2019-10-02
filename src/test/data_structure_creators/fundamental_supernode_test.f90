program fundamental_supernode_test
  use jagged_array_m
  use contiguous_sets_m
  use fundamental_supernode_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs, tree_child, ccs_supernode
  integer, pointer, contiguous :: col(:), row(:), num_child(:), child_val(:)
  integer, pointer, contiguous :: isleaf(:), num_child_supernode(:), first_node(:), num_child_check(:)
  type(contiguous_sets_c), pointer :: node_sets
  integer, pointer, contiguous :: cc_node(:), cc_supernode(:)
  integer :: i

  call make_ccs_postordering(col, row)
  ccs => create_jagged_array(col, row)
  call make_postordering_tree(num_child=num_child, child_val=child_val)
  tree_child => create_jagged_array(num_child, child_val)
  
  allocate(isleaf(9))
  isleaf = (/1, 1, 1, 0, 1, 1, 0, 0, 0/)

  node_sets => search_node_sets_in_supernode(isleaf, tree_child)
  allocate(first_node(8))
  do i=1, 8
    first_node(i) = node_sets%get_first(i)
  enddo
  call assert_equal("first node", first_node, (/1, 2, 3, 5, 6, 7, 8, 10/))


  num_child_supernode => create_supernodal_tree(node_sets, tree_child)
  call make_supernodal_tree(num_child_check)
  call assert_equal("supernodal tree", num_child_supernode, num_child_check)

  ccs_supernode => create_supernodal_ccs(node_sets, ccs)
  
  call assert_equal("supernodal ccs(1)", ccs_supernode%get_array(1), (/2, 8/))
  call assert_equal("supernodal ccs(2)", ccs_supernode%get_array(2), (/9/))
  call assert_equal("supernodal ccs(3)", ccs_supernode%get_array(3), (/7, 9/))
  call assert_equal("supernodal ccs(4)", ccs_supernode%get_array(4), (/6, 8/))
  call assert_equal("supernodal ccs(5)", ccs_supernode%get_array(5), (/7, 8/))
  call assert_equal("supernodal ccs(6)", ccs_supernode%get_array(6), (/9/))

  allocate(cc_node(9))
  cc_node = (/3, 3, 4, 3, 3, 3, 3, 2, 1/)
  cc_supernode => create_supernodal_column_count(node_sets, cc_node)
  call assert_equal("column count in supernode", cc_supernode, (/2, 2, 2, 2, 2, 2, 0/))

  
end program fundamental_supernode_test