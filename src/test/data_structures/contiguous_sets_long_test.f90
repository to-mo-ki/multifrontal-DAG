program contiguous_sets_long_test
  use contiguous_sets_long_m
  use test_util
  implicit none

  integer, pointer, contiguous :: length(:)
  type(contiguous_sets_long_c), pointer :: set

  allocate(length, source=[1000000000, 1000000000, 1000000000])
  set => create_contiguous_sets_long(length)

  call assert_equal("num sets", set%get_num_sets(), 3)
  call assert_equal("num elements", set%get_num_elements(), 3000000000)
  
  call start_tests("get_first")
  call add_test("index=1", set%get_first(1), 1)
  call add_test("index=2", set%get_first(2), 1000000001)
  call add_test("index=3", set%get_first(3), 2000000001)
  call end_tests()
  
  call start_tests("get_last")
  call add_test("index=1", set%get_last(1), 1000000000)
  call add_test("index=2", set%get_last(2), 2000000000)
  call add_test("index=3", set%get_last(3), 3000000000)
  call end_tests()

  call start_tests("get_length")
  call add_test("index=1", set%get_length(1), 1000000000)
  call add_test("index=2", set%get_length(2), 1000000000)
  call add_test("index=3", set%get_length(3), 1000000000)
  call end_tests()

end program