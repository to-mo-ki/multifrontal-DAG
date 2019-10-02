program column_count_test
  use jagged_array_m
  use column_count_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs, tree_child
  integer, pointer, contiguous :: num_row(:), row(:), cc(:), num_child(:), child_val(:), parent(:)

  call make_ccs_postordering(num_row, row)
  ccs => create_jagged_array(num_row, row)

  call make_postordering_tree(parent, num_child, child_val)
  tree_child => create_jagged_array(num_child, child_val)
  
  cc => column_count(ccs, tree_child, parent)

  call assert_equal("column count", cc, (/3, 3, 4, 3, 3, 3, 3, 2, 1/))

  
end program column_count_test