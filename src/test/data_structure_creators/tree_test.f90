program tree_test
  use jagged_array_m
  use tree_m
  use tree_maker_m
  use test_util
  implicit none

  integer, pointer, contiguous :: num_child(:), parent(:), perm(:), subtree_size(:)
  integer, pointer, contiguous :: check_postordering_parent(:), check_child_val(:)
  type(jagged_array_c), pointer :: tree_child
  integer :: n

  n = 9
  call make_original_tree(parent, num_child, check_child_val)
  tree_child => create_tree_child(num_child, parent)
  
  call assert_equal("tree_child by parent and num_child", tree_child%get_val(), check_child_val)

  parent => create_parent(tree_child)
  call assert_equal("create_parent", parent, [7, 4, 5, 6, 6, 8, 8, 9, 0])

  subtree_size => count_subtree_size(tree_child)
  call assert_equal("subtree size", subtree_size, [1, 1, 1, 2, 2, 5, 2, 8, 9])
  perm => tree_traverse_postordering(tree_child)
  call assert_equal("postordering", perm, [2, 4, 3, 5, 6, 1, 7, 8, 9])

  tree_child => create_tree_child(parent)

  call assert_equal("tree_child by parent", tree_child%get_val(), check_child_val)

  deallocate(num_child)
  call make_postordering_tree(parent=check_postordering_parent, num_child=num_child)
  parent => create_parent_in_postordering_tree(num_child)
  call assert_equal("create_parent_in_postordering_tree", parent, check_postordering_parent)

end program 