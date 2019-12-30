program reordering_test
  use jagged_array_m
  use reordering_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use perm_maker_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs_origin, ccs_reordered
  integer, pointer, contiguous :: perm(:), iperm(:), col(:), row(:), parent_origin(:), parent_reordered(:)
  integer, pointer, contiguous :: check_parent(:), ccs_perm(:)
  double precision, pointer, contiguous :: origin_val(:), ordered_val(:), check_val(:)
  
  call make_postordering_perm(perm, iperm)
  
  allocate(parent_origin(9))
  call make_original_tree(parent_origin)
  parent_reordered => reordering_tree(parent_origin, perm, iperm)
  call make_postordering_tree(check_parent)
  call assert_equal("reordering_tree", parent_reordered, check_parent)

  call make_ccs(col, row)
  ccs_origin => create_jagged_array(col, row)
  ccs_reordered => repostordering_ccs(ccs_origin, perm, iperm)

  call assert_equal("repostordering_ccs num_row", ccs_reordered%get_array_lengths(), [3, 2, 4, 2, 3, 3, 2, 1, 1])
  call assert_equal("repostordering_ccs row", ccs_reordered%get_val(), [1, 2, 8, 2, 9, 3, 4, 7, 9, 4, 9, 5, 6, 8, 6, 7, 8, 7, 9, 8, 9])

  ccs_reordered => repostordering_ccs(ccs_origin, perm, iperm, ccs_perm)

  call assert_equal("repostordering_ccs:num_row", ccs_reordered%get_array_lengths(), [3,2,4,2,3,3,2,1,1])
  call assert_equal("repostordering_ccs:row", ccs_reordered%get_val(), [1,2,8,2,9,3,4,7,9,4,9,5,6,8,6,7,8,7,9,8,9])
  call assert_equal("repostordering_ccs_with_perm:ccs_perm", ccs_perm, [1,2,3,18,19,4,5,6,7,11,12,8,9,10,13,14,15,16,17,20,21])

  allocate(origin_val(21))
  origin_val = [11,71,81,22,42,62,92,33,53,83,44,94,55,65,85,66,96,77,97,88,99]
  ordered_val => reordering_ccs_val(ccs_origin%get_set(), ccs_reordered%get_set(), origin_val, perm)
  allocate(check_val(21))
  check_val = [11,71,81,77,97,22,42,62,92,44,94,33,53,83,55,65,85,66,96,88,99]
  call assert_equal("reordering_ccs_val", ordered_val, check_val)

  ccs_origin => create_jagged_array([3,2,1],[1,2,3,2,3,3])
  ccs_reordered => reordering_ccs(ccs_origin, [3,1,2], [2,3,1], ccs_perm)

  call assert_equal("repostordering_ccs:num_row", ccs_reordered%get_array_lengths(), [3,2,1])
  call assert_equal("repostordering_ccs:row", ccs_reordered%get_val(), [1,2,3,2,3,3])
  call assert_equal("repostordering_ccs_with_perm:ccs_perm", ccs_perm, [6,3,5,1,2,4])

  
end program reordering_test