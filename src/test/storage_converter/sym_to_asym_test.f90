program sym_to_asym_test
  use jagged_array_m
  use sym_to_asym_m
  use sparse_matrix_maker_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: sym, asym
  integer, pointer, contiguous :: col(:), row(:)

  call make_ccs(col, row)
  sym => create_jagged_array(col, row)
  call sym_to_asym(sym, asym)

  call assert_equal("asym row", asym%get_array_lengths(), (/2, 3, 2, 2, 3, 3, 2, 3, 4/))
  call assert_equal("asym col", asym%get_val(), (/7, 8, 4, 6, 9, 5, 8, 2, 9, 3, 6, 8, 2, 5, 9, 1, 9, 1, 3, 5, 2, 4, 6, 7/))


  
end program sym_to_asym_test