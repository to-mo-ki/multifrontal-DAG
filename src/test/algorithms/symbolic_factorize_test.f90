program symbolic_factorize_test
  use jagged_array_m
  use contiguous_sets_m
  use symbolic_factorize_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs_a, ccs_l, tree_child
  integer, pointer, contiguous :: num_row(:), row(:), cc(:), num_child(:), child_val(:)
  type(contiguous_sets_c), pointer :: node_sets

  allocate(num_row(7), row(11))
  num_row = (/2, 1, 2, 2, 2, 2, 0/)
  row = (/2, 8, 9, 7, 9, 6, 8, 7, 8, 8, 9/)
  ccs_a => create_jagged_array(num_row, row)
  node_sets => create_contiguous_sets((/1, 1, 2, 1, 1, 1, 2/))

  allocate(cc(7))
  cc = (/2, 2, 2, 2, 2, 2, 0/)

  allocate(num_child(7), child_val(6))
  num_child = (/0, 1, 0, 0, 1, 2, 2/)
  child_val = (/1, 4, 3, 5, 2, 6/)
  tree_child => create_jagged_array(num_child, child_val)

  ccs_l => symbolic_factorize(ccs_a, node_sets, cc, tree_child)

  call assert_equal("col(1)", ccs_l%get_array(1), (/2, 8/))
  call assert_equal("col(2)", ccs_l%get_array(2), (/8, 9/))
  call assert_equal("col(3)", ccs_l%get_array(3), (/7, 9/))
  call assert_equal("col(4)", ccs_l%get_array(4), (/6, 8/))
  call assert_equal("col(5)", ccs_l%get_array(5), (/7, 8/))
  call assert_equal("col(6)", ccs_l%get_array(6), (/8, 9/))
  call assert_equal("col(7)", ccs_l%get_array_length(7), 0)
  
end program symbolic_factorize_test