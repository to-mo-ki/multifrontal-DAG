program ccs_test
  use ccs_m
  use jagged_array_m
  use test_util
  use sparse_matrix_maker_m
  implicit none
  type(jagged_array_c), pointer :: structure
  type(ccs_c), pointer :: ccs
  integer, pointer, contiguous :: col(:), row(:)
  double precision, pointer, contiguous :: row_val(:)
  integer :: i
  call make_ccs(col, row)
  allocate(row_val(size(row)))
  do i=1, size(row)
    row_val(i) = dble(row(i))
  enddo

  structure => create_jagged_array(col, row)
  ccs => create_ccs(structure, row_val)

  call start_array_tests("get_row_array")
  call add_test("index=1", ccs%get_row_array(1), [1, 7, 8])
  call add_test("index=2", ccs%get_row_array(2), [2, 4, 6, 9])
  call add_test("index=3", ccs%get_row_array(3), [3, 5, 8])
  call add_test("index=4", ccs%get_row_array(4), [4, 9])
  call add_test("index=5", ccs%get_row_array(5), [5, 6, 8])
  call add_test("index=6", ccs%get_row_array(6), [6, 9])
  call add_test("index=7", ccs%get_row_array(7), [7, 9])
  call add_test("index=8", ccs%get_row_array(8), [8])
  call add_test("index=9", ccs%get_row_array(9), [9])
  call end_array_tests()

  call start_array_tests("get_val_array")
  call add_test("index=1", ccs%get_val_array(1), [1d0, 7d0, 8d0])
  call add_test("index=2", ccs%get_val_array(2), [2d0, 4d0, 6d0, 9d0])
  call add_test("index=3", ccs%get_val_array(3), [3d0, 5d0, 8d0])
  call add_test("index=4", ccs%get_val_array(4), [4d0, 9d0])
  call add_test("index=5", ccs%get_val_array(5), [5d0, 6d0, 8d0])
  call add_test("index=6", ccs%get_val_array(6), [6d0, 9d0])
  call add_test("index=7", ccs%get_val_array(7), [7d0, 9d0])
  call add_test("index=8", ccs%get_val_array(8), [8d0])
  call add_test("index=9", ccs%get_val_array(9), [9d0])
  call end_array_tests()

  
end program ccs_test