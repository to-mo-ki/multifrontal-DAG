program compute_tree_test
  use jagged_array_m
  use compute_tree_m
  use sparse_matrix_maker_m
  use test_util
  implicit none
  type(jagged_array_c) :: crs
  integer, pointer, contiguous :: row(:), col(:), parent(:)

  call make_crs(row, col)
  crs = create_jagged_array(row, col)
  parent => compute_tree(crs)
  call assert_equal("compute tree", parent, (/7, 4, 5, 6, 6, 8, 8, 9, 0/), 9)

  
end program compute_tree_test