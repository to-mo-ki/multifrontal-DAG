program reordering_test
  use jagged_array_m
  use reordering_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs_origin, ccs_reordered
  integer, pointer, contiguous :: perm(:), iperm(:), col(:), row(:), parent_origin(:), parent_reordered(:)
  
  allocate(perm(9), iperm(9))
  perm = (/1, 7, 2, 4, 3, 5, 6, 8, 9/)
  iperm = (/1, 3, 5, 4, 6, 7, 2, 8, 9/)

  allocate(parent_origin(9))
  parent_origin = (/7, 4, 5, 6, 6, 8, 8, 9, 0/)
  parent_reordered => reordering_tree(parent_origin, perm, iperm)
  call assert_equal("reordering_tree", parent_reordered, (/2, 8, 4, 7, 6, 7, 8, 9, 0/), 9)

  call make_ccs(col, row)
  ccs_origin = create_jagged_array(col, row)
  ccs_reordered = reordering_ccs(ccs_origin, perm, iperm)
  
  call assert_equal("reordering_ccs:column count(1)", ccs_reordered%get_array_length(1), 3)
  call assert_equal("reordering_ccs:column count(2)", ccs_reordered%get_array_length(2), 2)
  call assert_equal("reordering_ccs:column count(3)", ccs_reordered%get_array_length(3), 4)
  call assert_equal("reordering_ccs:column count(4)", ccs_reordered%get_array_length(4), 2)
  call assert_equal("reordering_ccs:column count(5)", ccs_reordered%get_array_length(5), 3)
  call assert_equal("reordering_ccs:column count(6)", ccs_reordered%get_array_length(6), 3)
  call assert_equal("reordering_ccs:column count(7)", ccs_reordered%get_array_length(7), 2)
  call assert_equal("reordering_ccs:column count(8)", ccs_reordered%get_array_length(8), 1)
  call assert_equal("reordering_ccs:column count(9)", ccs_reordered%get_array_length(9), 1)

  call assert_equal("reordering_ccs:rows(1)", ccs_reordered%get_array(1), (/1, 2, 8/), 3)
  call assert_equal("reordering_ccs:rows(2)", ccs_reordered%get_array(2), (/2, 9/), 2)
  call assert_equal("reordering_ccs:rows(3)", ccs_reordered%get_array(3), (/3, 4, 7, 9/), 4)
  call assert_equal("reordering_ccs:rows(4)", ccs_reordered%get_array(4), (/4, 9/), 2)
  call assert_equal("reordering_ccs:rows(5)", ccs_reordered%get_array(5), (/5, 6, 8/), 3)
  call assert_equal("reordering_ccs:rows(6)", ccs_reordered%get_array(6), (/6, 7, 8/), 3)
  call assert_equal("reordering_ccs:rows(7)", ccs_reordered%get_array(7), (/7, 9/), 2)
  call assert_equal("reordering_ccs:rows(8)", ccs_reordered%get_array(8), (/8/), 1)
  call assert_equal("reordering_ccs:rows(9)", ccs_reordered%get_array(9), (/9/), 1)
  



  
end program reordering_test