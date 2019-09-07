program sym_to_asym_test
  use jagged_array_m
  use sym_to_asym_m
  use sparse_matrix_maker_m
  use test_util
  implicit none
  type(jagged_array_c) :: sym, asym
  integer, pointer, contiguous :: col(:), row(:)

  call make_ccs(col, row)
  sym = create_jagged_array(col, row)
  call sym_to_asym(sym, asym)

  call assert_equal("asym_col(1)", asym%get_array_length(1), 2)
  call assert_equal("asym_col(2)", asym%get_array_length(2), 3)
  call assert_equal("asym_col(3)", asym%get_array_length(3), 2)
  call assert_equal("asym_col(4)", asym%get_array_length(4), 2)
  call assert_equal("asym_col(5)", asym%get_array_length(5), 3)
  call assert_equal("asym_col(6)", asym%get_array_length(6), 3)
  call assert_equal("asym_col(7)", asym%get_array_length(7), 2)
  call assert_equal("asym_col(8)", asym%get_array_length(8), 3)
  call assert_equal("asym_col(9)", asym%get_array_length(9), 4)

  call assert_equal("asym_row(1)", asym%get_array(1), (/7, 8/), asym%get_array_length(1))
  call assert_equal("asym_row(2)", asym%get_array(2), (/4, 6, 9/), asym%get_array_length(2))
  call assert_equal("asym_row(3)", asym%get_array(3), (/5, 8/), asym%get_array_length(3))
  call assert_equal("asym_row(4)", asym%get_array(4), (/2, 9/), asym%get_array_length(4))
  call assert_equal("asym_row(5)", asym%get_array(5), (/3, 6, 8/), asym%get_array_length(5))
  call assert_equal("asym_row(6)", asym%get_array(6), (/2, 5, 9/), asym%get_array_length(6))
  call assert_equal("asym_row(7)", asym%get_array(7), (/1, 9/), asym%get_array_length(7))
  call assert_equal("asym_row(8)", asym%get_array(8), (/1, 3, 5/), asym%get_array_length(8))
  call assert_equal("asym_row(9)", asym%get_array(9), (/2, 4, 6, 7/), asym%get_array_length(9))


  
end program sym_to_asym_test