program supernode_test
  use jagged_array_m
  use supernode_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs, tree_child, ccs_supernode
  integer, pointer, contiguous :: col(:), row(:), child_ptr(:), child_val(:)
  integer, pointer, contiguous :: isleaf(:), first_node(:), num_child_supernode(:)

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
  call assert_equal("first node", first_node, (/1, 2, 3, 5, 6, 7, 8, 10/), 8)


  num_child_supernode => create_supernodal_tree(first_node, tree_child)
  call assert_equal("supernodal tree", num_child_supernode, (/0, 1, 0, 0, 1, 2, 2/), 7)

  ccs_supernode = create_supernodal_ccs(first_node, ccs)

  call assert_equal("supernodal ccs:N", ccs_supernode%get_num_arrays(), 7)
  call assert_equal("supernodal ccs:col(1)", ccs_supernode%get_array_length(1), 2)
  call assert_equal("supernodal ccs:col(2)", ccs_supernode%get_array_length(2), 1)
  call assert_equal("supernodal ccs:col(3)", ccs_supernode%get_array_length(3), 2)
  call assert_equal("supernodal ccs:col(4)", ccs_supernode%get_array_length(4), 2)
  call assert_equal("supernodal ccs:col(5)", ccs_supernode%get_array_length(5), 2)
  call assert_equal("supernodal ccs:col(6)", ccs_supernode%get_array_length(6), 1)
  call assert_equal("supernodal ccs:col(7)", ccs_supernode%get_array_length(7), 0)

  
  call assert_equal("supernodal ccs:row(1)", ccs_supernode%get_array(1), (/2, 8/), 2)
  call assert_equal("supernodal ccs:row(2)", ccs_supernode%get_array(2), (/9/), 1)
  call assert_equal("supernodal ccs:row(3)", ccs_supernode%get_array(3), (/7, 9/), 2)
  call assert_equal("supernodal ccs:row(4)", ccs_supernode%get_array(4), (/6, 8/), 2)
  call assert_equal("supernodal ccs:row(5)", ccs_supernode%get_array(5), (/7, 8/), 2)
  call assert_equal("supernodal ccs:row(6)", ccs_supernode%get_array(6), (/9/), 1)
  
end program supernode_test