program compute_tree_test
  use jagged_array_m
  use compute_tree_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: crs
  integer, pointer, contiguous :: row(:), col(:), parent(:), check_parent(:)

  call make_crs(row, col)
  crs => create_jagged_array(row, col)
  parent => compute_tree(crs)
  call make_original_tree(check_parent)
  call assert_equal("compute tree", parent, check_parent)

  
end program compute_tree_test