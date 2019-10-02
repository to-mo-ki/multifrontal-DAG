program tree_pruning_test
  use jagged_array_m
  use contiguous_sets_m
  use tree_pruning_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util

  implicit none

  integer, pointer, contiguous :: cc(:), subtree(:)
  type(jagged_array_c), pointer :: tree_child
  type(contiguous_sets_c), pointer :: node_sets
  integer, pointer, contiguous :: num_child(:), child(:), node_size(:)

  allocate(node_size(7))
  call make_supernodal_ccs(cc, node_size=node_size)
  call make_supernodal_tree(num_child, child)
  
  tree_child => create_jagged_array(num_child, child)
  node_sets => create_contiguous_sets(node_size)
  
  subtree => tree_pruning(cc, node_sets, tree_child, 0d0, 1)
  call assert_equal("tol=0, p=1", subtree, (/7/))

  subtree => tree_pruning(cc, node_sets, tree_child, 0d0, 2)
  call assert_equal("tol=0, p=2", subtree, (/6, 2/))

  subtree => tree_pruning(cc, node_sets, tree_child, 0.71d0, 2)
  call assert_equal("tol=0.71, p=2", subtree, (/5, 3, 2/))

  subtree => tree_pruning(cc, node_sets, tree_child, 0.73d0, 2)
  call assert_equal("tol=0.73, p=2", subtree, (/5, 3, 2/))
  
  
end program tree_pruning_test