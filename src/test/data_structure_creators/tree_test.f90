program tree_test
  use jagged_array_m
  use tree_m
  use test_util
  implicit none

  integer, pointer, contiguous :: num_child(:), parent(:), perm(:), subtree_size(:)
  type(jagged_array_c), pointer :: tree_child
  integer :: i, n

  n = 9
  allocate(num_child(n), parent(n))
  num_child = (/0, 0, 0, 1, 1, 2, 1, 2, 1/)
  parent = (/7, 4, 5, 6, 6, 8, 8, 9, 0/)
  tree_child => create_tree_child(num_child, parent)
  
  call assert_equal("tree_child by parent and num_child:array(4)", tree_child%get_array(4), (/2/))
  call assert_equal("tree_child by parent and num_child:array(5)", tree_child%get_array(5), (/3/))
  call assert_equal("tree_child by parent and num_child:array(6)", tree_child%get_array(6), (/4, 5/))
  call assert_equal("tree_child by parent and num_child:array(7)", tree_child%get_array(7), (/1/))
  call assert_equal("tree_child by parent and num_child:array(8)", tree_child%get_array(8), (/6, 7/))
  call assert_equal("tree_child by parent and num_child:array(9)", tree_child%get_array(9), (/8/))

  subtree_size => count_subtree_size(tree_child)
  call assert_equal("subtree size", subtree_size, (/1, 1, 1, 2, 2, 5, 2, 8, 9/))
  perm => tree_traverse_postordering(tree_child)
  call assert_equal("postordering", perm, (/2, 4, 3, 5, 6, 1, 7, 8, 9/))

  tree_child => create_tree_child(parent)

  call assert_equal("tree_child by parent:array(4)", tree_child%get_array(4), (/2/))
  call assert_equal("tree_child by parent:array(5)", tree_child%get_array(5), (/3/))
  call assert_equal("tree_child by parent:array(6)", tree_child%get_array(6), (/4, 5/))
  call assert_equal("tree_child by parent:array(7)", tree_child%get_array(7), (/1/))
  call assert_equal("tree_child by parent:array(8)", tree_child%get_array(8), (/6, 7/))
  call assert_equal("tree_child by parent:array(9)", tree_child%get_array(9), (/8/))

  num_child = (/0, 1, 0, 1, 0, 1, 2, 2, 1/)
  parent => create_parent_in_postordering_tree(num_child)
  call assert_equal("create_parent_in_postordering_tree", parent, (/2, 8, 4, 7, 6, 7, 8, 9, 0/))

end program 