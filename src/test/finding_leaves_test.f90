program finding_leaves_test
  use jagged_array_m
  use finding_leaves_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs
  integer, pointer, contiguous :: col(:), row(:), subtree_size(:), isleaf(:)
  call make_ccs_postordering(col, row)
  ccs = create_jagged_array(col, row)
  allocate(subtree_size(9))
  subtree_size = (/1, 2, 1, 2, 1, 2, 5, 8, 9/)
  isleaf => finding_leaves(subtree_size, ccs)
  call assert_equal("finding_leaves", isleaf, (/1, 1, 1, 0, 1, 1, 0, 0, 0/))
  
end program finding_leaves_test