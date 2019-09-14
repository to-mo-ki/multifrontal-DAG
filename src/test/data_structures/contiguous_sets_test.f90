program contiguous_sets_test
  use contiguous_sets_m
  use test_util
  implicit none

  integer, pointer, contiguous :: length(:)
  type(contiguous_sets_c), pointer :: set
  allocate(length(4))

  length = (/2, 1, 0, 3/)
  set => create_contiguous_sets(length)

  call assert_equal("num sets", set%get_num_sets(), 4)
  call assert_equal("num elements", set%get_num_elements(), 6)
  call assert_equal("first:1", set%get_first(1), 1)
  call assert_equal("first:2", set%get_first(2), 3)
  call assert_equal("first:4", set%get_first(4), 4)
  call assert_equal("last:1", set%get_last(1), 2)
  call assert_equal("last:2", set%get_last(2), 3)
  call assert_equal("last:4", set%get_last(4), 6)
  call assert_equal("length:1", set%get_length(1), 2)
  call assert_equal("length:2", set%get_length(2), 1)
  call assert_equal("length:3", set%get_length(3), 0)
  call assert_equal("length:4", set%get_length(4), 3)
  

end program