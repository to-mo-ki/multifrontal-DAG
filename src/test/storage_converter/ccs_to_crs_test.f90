program ccs_to_crs_test
  use jagged_array_m
  use ccs_to_crs_m
  use sparse_matrix_maker_m
  use test_util
  implicit none
  type(jagged_array_c) :: ccs, crs
  integer :: n
  integer, pointer, contiguous :: cols(:)

  n = 9
  call make_ccs(ccs)
  call ccs_to_crs(ccs, crs)
  
  call assert_equal("crs_col(1)", crs%get_array_length(1), 1)
  call assert_equal("crs_col(2)", crs%get_array_length(2), 1)
  call assert_equal("crs_col(3)", crs%get_array_length(3), 1)
  call assert_equal("crs_col(4)", crs%get_array_length(4), 2)
  call assert_equal("crs_col(5)", crs%get_array_length(5), 2)
  call assert_equal("crs_col(6)", crs%get_array_length(6), 3)
  call assert_equal("crs_col(7)", crs%get_array_length(7), 2)
  call assert_equal("crs_col(8)", crs%get_array_length(8), 4)
  call assert_equal("crs_col(9)", crs%get_array_length(9), 5)

  call assert_equal_array("crs_row(1)", crs%get_array(1), crs%get_array_length(1), (/1/))
  call assert_equal_array("crs_row(2)", crs%get_array(2), crs%get_array_length(2), (/2/))
  call assert_equal_array("crs_row(3)", crs%get_array(3), crs%get_array_length(3), (/3/))
  call assert_equal_array("crs_row(4)", crs%get_array(4), crs%get_array_length(4), (/2, 4/))
  call assert_equal_array("crs_row(5)", crs%get_array(5), crs%get_array_length(5), (/3, 5/))
  call assert_equal_array("crs_row(6)", crs%get_array(6), crs%get_array_length(6), (/2, 5, 6/))
  call assert_equal_array("crs_row(7)", crs%get_array(7), crs%get_array_length(7), (/1, 7/))
  call assert_equal_array("crs_row(8)", crs%get_array(8), crs%get_array_length(8), (/1, 3, 5, 8/))
  call assert_equal_array("crs_row(9)", crs%get_array(9), crs%get_array_length(9), (/2, 4, 6, 7, 9/))
  
end program ccs_to_crs_test