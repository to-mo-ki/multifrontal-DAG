program reordering_test
  use jagged_array_m
  use reordering_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs_origin, ccs_reordered
  integer, pointer, contiguous :: perm(:), iperm(:), col(:), row(:), parent_origin(:), parent_reordered(:)
  
  allocate(perm(9), iperm(9))
  perm = (/1, 7, 2, 4, 3, 5, 6, 8, 9/)
  iperm = (/1, 3, 5, 4, 6, 7, 2, 8, 9/)

  allocate(parent_origin(9))
  call make_original_tree(parent_origin)
  parent_reordered => reordering_tree(parent_origin, perm, iperm)
  call assert_equal("reordering_tree", parent_reordered, (/2, 8, 4, 7, 6, 7, 8, 9, 0/))

  call make_ccs(col, row)
  ccs_origin => create_jagged_array(col, row)
  ccs_reordered => reordering_ccs(ccs_origin, perm, iperm)

  call assert_equal("reordering_ccs(1)", ccs_reordered%get_array(1), (/1, 2, 8/))
  call assert_equal("reordering_ccs(2)", ccs_reordered%get_array(2), (/2, 9/))
  call assert_equal("reordering_ccs(3)", ccs_reordered%get_array(3), (/3, 4, 7, 9/))
  call assert_equal("reordering_ccs(4)", ccs_reordered%get_array(4), (/4, 9/))
  call assert_equal("reordering_ccs(5)", ccs_reordered%get_array(5), (/5, 6, 8/))
  call assert_equal("reordering_ccs(6)", ccs_reordered%get_array(6), (/6, 7, 8/))
  call assert_equal("reordering_ccs(7)", ccs_reordered%get_array(7), (/7, 9/))
  call assert_equal("reordering_ccs(8)", ccs_reordered%get_array(8), (/8/))
  call assert_equal("reordering_ccs(9)", ccs_reordered%get_array(9), (/9/))
  



  
end program reordering_test