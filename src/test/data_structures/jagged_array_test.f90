program jagged_array_test
  use sparse_matrix_maker_m
  use test_util
  use jagged_array_m
  implicit none
  type(jagged_array_c) :: ccs
  integer :: i, j
  integer, pointer, contiguous :: col(:), row(:)

  call make_ccs(col, row)
  ccs = create_jagged_array(col, row)
  call assert_equal("get_num_arrays", ccs%get_num_arrays(), 9)
  call assert_equal("get_num_vals", ccs%get_num_vals(), 21)


  call assert_equal("get_array_length(1)", ccs%get_array_length(1), 3)
  call assert_equal("get_array_length(2)", ccs%get_array_length(2), 4)
  call assert_equal("get_array_length(3)", ccs%get_array_length(3), 3)
  call assert_equal("get_array_length(4)", ccs%get_array_length(4), 2)
  call assert_equal("get_array_length(5)", ccs%get_array_length(5), 3)
  call assert_equal("get_array_length(6)", ccs%get_array_length(6), 2)
  call assert_equal("get_array_length(7)", ccs%get_array_length(7), 2)
  call assert_equal("get_array_length(8)", ccs%get_array_length(8), 1)
  call assert_equal("get_array_length(9)", ccs%get_array_length(9), 1)

  call assert_equal("get_array(1)", ccs%get_array(1), (/1, 7, 8/), 3)
  call assert_equal("get_array(2)", ccs%get_array(2), (/2, 4, 6, 9/), 4)
  call assert_equal("get_array(3)", ccs%get_array(3), (/3, 5, 8/), 3)
  call assert_equal("get_array(4)", ccs%get_array(4), (/4, 9/), 2)
  call assert_equal("get_array(5)", ccs%get_array(5), (/5, 6, 8/), 3)
  call assert_equal("get_array(6)", ccs%get_array(6), (/6, 9/), 2)
  call assert_equal("get_array(7)", ccs%get_array(7), (/7, 9/), 2)
  call assert_equal("get_array(8)", ccs%get_array(8), (/8/), 1)
  call assert_equal("get_array(9)", ccs%get_array(9), (/9/), 1)
  
  
end program