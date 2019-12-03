program jagged_array_test
  use sparse_matrix_maker_m
  use test_util
  use jagged_array_DP_m
  use contiguous_sets_m
  implicit none
  type(jagged_array_DP_c), pointer :: ccs
  type(contiguous_sets_c), pointer :: set
  integer, pointer, contiguous :: col(:), row(:)
  double precision, pointer, contiguous :: row_val(:)
  integer :: i
  call make_ccs(col, row)
  allocate(row_val(size(row)))
  do i=1, size(row)
    row_val(i) = dble(row(i))
  enddo
  set => create_contiguous_sets(col)
  ccs => create_jagged_array_DP(set, row_val)

  call start_array_tests("get_array")
  call add_test("index=1", ccs%get_array(1), [1d0, 7d0, 8d0])
  call add_test("index=2", ccs%get_array(2), [2d0, 4d0, 6d0, 9d0])
  call add_test("index=3", ccs%get_array(3), [3d0, 5d0, 8d0])
  call add_test("index=4", ccs%get_array(4), [4d0, 9d0])
  call add_test("index=5", ccs%get_array(5), [5d0, 6d0, 8d0])
  call add_test("index=6", ccs%get_array(6), [6d0, 9d0])
  call add_test("index=7", ccs%get_array(7), [7d0, 9d0])
  call add_test("index=8", ccs%get_array(8), [8d0])
  call add_test("index=9", ccs%get_array(9), [9d0])
  call end_array_tests()
  
end program