program jagged_array_test
  use sparse_matrix_maker_m
  use test_util
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  type(jagged_array_c), pointer :: ccs
  type(contiguous_sets_c), pointer :: set
  integer, pointer, contiguous :: col(:), row(:), val(:)

  call make_ccs(col, row)
  ccs => create_jagged_array(col, row)
  call assert_equal("get_num_arrays", ccs%get_num_arrays(), 9)
  call assert_equal("get_num_vals", ccs%get_num_vals(), 21)

  call start_array_tests("get_array")
  call add_test("get_array(1)", ccs%get_array(1), [1, 7, 8])
  call add_test("get_array(2)", ccs%get_array(2), [2, 4, 6, 9])
  call add_test("get_array(3)", ccs%get_array(3), [3, 5, 8])
  call add_test("get_array(4)", ccs%get_array(4), [4, 9])
  call add_test("get_array(5)", ccs%get_array(5), [5, 6, 8])
  call add_test("get_array(6)", ccs%get_array(6), [6, 9])
  call add_test("get_array(7)", ccs%get_array(7), [7, 9])
  call add_test("get_array(8)", ccs%get_array(8), [8])
  call add_test("get_array(9)", ccs%get_array(9), [9])
  call end_array_tests()
  call assert_equal("get_array_lengths", ccs%get_array_lengths(), [3, 4, 3, 2, 3, 2, 2, 1, 1])
  call assert_equal("get_val", ccs%get_val(), row)
  val => ccs%get_raw_val()
  call assert_equal("get_raw_val", ccs%get_raw_val(), row)
  set => ccs%get_set()
  call start_tests("get_length")
  call add_test("1", set%get_length(1), 3)
  call add_test("2", set%get_length(2), 4)
  call add_test("3", set%get_length(3), 3)
  call add_test("4", set%get_length(4), 2)
  call add_test("5", set%get_length(5), 3)
  call add_test("6", set%get_length(6), 2)
  call add_test("7", set%get_length(7), 2)
  call add_test("8", set%get_length(8), 1)
  call add_test("9", set%get_length(9), 1)
  call end_tests()

  ccs => create_jagged_array(set)
  call assert_equal("get_num_arrays2", ccs%get_num_arrays(), 9)
  call assert_equal("get_num_vals2", ccs%get_num_vals(), 21)

end program