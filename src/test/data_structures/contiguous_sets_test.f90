program contiguous_sets_test
  use contiguous_sets_m
  use test_util
  implicit none

  integer, pointer, contiguous :: length(:)
  type(contiguous_sets_c), pointer :: set
  allocate(length(4))

  length = [2, 1, 0, 3]
  set => create_contiguous_sets(length)

  call assert_equal("num sets", set%get_num_sets(), 4)
  call assert_equal("num elements", set%get_num_elements(), 6)
  
  call start_tests("get_first")
  call add_test("index=1", set%get_first(1), 1)
  call add_test("index=2", set%get_first(2), 3)
  call add_test("index=4", set%get_first(4), 4)
  call end_tests()
  
  call start_tests("get_last")
  call add_test("index=1", set%get_last(1), 2)
  call add_test("index=2", set%get_last(2), 3)
  call add_test("index=4", set%get_last(4), 6)
  call end_tests()

  call start_tests("get_length")
  call add_test("index=1", set%get_length(1), 2)
  call add_test("index=2", set%get_length(2), 1)
  call add_test("index=3", set%get_length(3), 0)
  call add_test("index=4", set%get_length(4), 3)
  call end_tests()

  set => create_raw_contiguous_sets([1,3,4,4,7],4)
  call start_tests("raw_constructor by first & last")
  call add_test("first:index=1", set%get_first(1), 1)
  call add_test("first:index=2", set%get_first(2), 3)
  call add_test("first:index=4", set%get_first(4), 4)
  call add_test("last:index=1", set%get_last(1), 2)
  call add_test("last:index=2", set%get_last(2), 3)
  call add_test("last:index=4", set%get_last(4), 6)
  call end_tests()

end program