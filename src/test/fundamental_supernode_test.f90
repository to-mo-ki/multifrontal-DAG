program fundamental_supernode_test
  use jagged_array_m
  use contiguous_sets_m
  use fundamental_supernode_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs, tree_child, ccs_supernode
  integer, pointer, contiguous :: col(:), row(:), child_ptr(:), child_val(:)
  integer, pointer, contiguous :: isleaf(:), num_child_supernode(:), first_node(:)
  type(contiguous_sets_c) :: node_sets
  integer, pointer, contiguous :: cc_node(:), cc_supernode(:)
  integer :: i

  call make_ccs_postordering(col, row)
  ccs = create_jagged_array(col, row)
  allocate(child_ptr(9))
  allocate(child_val(8))
  child_ptr = (/0, 1, 0, 1, 0, 1, 2, 2, 1/)
  child_val = (/1, 3, 5, 4, 6, 2, 7, 8/)
  tree_child = create_jagged_array(child_ptr, child_val)
  allocate(isleaf(9))
  isleaf = (/1, 1, 1, 0, 1, 1, 0, 0, 0/)

  node_sets = search_node_sets_in_supernode(isleaf, tree_child)
  allocate(first_node(8))
  do i=1, 8
    first_node(i) = node_sets%get_first(i)
  enddo
  call assert_equal("first node", first_node, (/1, 2, 3, 5, 6, 7, 8, 10/))


  num_child_supernode => create_supernodal_tree(node_sets, tree_child)
  call assert_equal("supernodal tree", num_child_supernode, (/0, 1, 0, 0, 1, 2, 2/))

  allocate(cc_node(9))
  cc_node = (/3, 3, 4, 3, 3, 3, 3, 2, 1/)
  cc_supernode => create_supernodal_column_count(node_sets, cc_node)
  call assert_equal("column count in supernode", cc_supernode, (/2, 2, 2, 2, 2, 2, 0/))

  
end program fundamental_supernode_test