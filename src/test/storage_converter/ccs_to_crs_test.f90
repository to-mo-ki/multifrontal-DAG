program ccs_to_crs_test
  use jagged_array_m
  use ccs_to_crs_m
  use sparse_matrix_maker_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: ccs, crs
  integer :: n
  integer, pointer, contiguous :: num_row(:), row(:)

  n = 9
  call make_ccs(num_row, row)
  ccs => create_jagged_array(num_row, row)
  call ccs_to_crs(ccs, crs)
  call assert_equal("crs row", crs%get_array_lengths(), [1, 1, 1, 2, 2, 3, 2, 4, 5])
  call assert_equal("crs col", crs%get_val(), [1, 2, 3, 2, 4, 3, 5, 2, 5, 6, 1, 7, 1, 3, 5, 8, 2, 4, 6, 7, 9])
  
end program ccs_to_crs_test