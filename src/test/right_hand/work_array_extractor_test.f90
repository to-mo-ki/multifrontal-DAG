program work_array_extractor_test
  use array_extractor_m
  use work_array_extractor_m
  use node_data_m
  use test_util
  implicit none
  class(extractor_c), pointer :: extractor
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)
  
  allocate(work_extractor_c::extractor)
  
  print *, "nb=3, nc=5, nr=5"
  extractor%node_data => create_node_data([5,6,7,5,3,6],[5,4,4,4,6,0],3)
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 5)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=2", extractor%get_ptr(array, 1, 2), [(dble(i), i=1,1)])
  call add_test("index=3", extractor%get_ptr(array, 1, 3), [(dble(i), i=2,4)])
  call add_test("index=4", extractor%get_ptr(array, 1, 4), [(dble(i), i=5,5)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=4"
  array_size = extractor%estimate_size(2)
  call assert_equal("estimate_size", array_size, 4)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=3", extractor%get_ptr(array, 2, 3), [(dble(i), i=1,3)])
  call add_test("index=4", extractor%get_ptr(array, 2, 4), [(dble(i), i=4,4)])
  call end_array_tests()

  print *, "nb=3, nc=7, nr=4"
  array_size = extractor%estimate_size(3)
  call assert_equal("estimate_size", array_size, 4)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=3", extractor%get_ptr(array, 3, 3), [(dble(i), i=1,2)])
  call add_test("index=4", extractor%get_ptr(array, 3, 4), [(dble(i), i=3,4)])
  call end_array_tests()

  print *, "nb=3, nc=5, nr=4"
  array_size = extractor%estimate_size(4)
  call assert_equal("estimate_size", array_size, 4)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=2", extractor%get_ptr(array, 4, 2), [(dble(i), i=1,1)])
  call add_test("index=3", extractor%get_ptr(array, 4, 3), [(dble(i), i=2,4)])
  call end_array_tests()

  print *, "nb=3, nc=3, nr=6"
  array_size = extractor%estimate_size(5)
  call assert_equal("estimate_size", array_size, 6)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=2", extractor%get_ptr(array, 5, 2), [(dble(i), i=1,3)])
  call add_test("index=3", extractor%get_ptr(array, 5, 3), [(dble(i), i=4,6)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=0"
  array_size = extractor%estimate_size(6)
  call assert_equal("estimate_size", array_size, 0)
  
  extractor%node_data => create_node_data([2,5],[5,2],4)
  print *, "nb=4, nc=2, nr=5(nc < nb)"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 5)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=1", extractor%get_ptr(array, 1, 1), [(dble(i), i=1,2)])
  call add_test("index=2", extractor%get_ptr(array, 1, 2), [(dble(i), i=3,5)])
  call end_array_tests()


  print *, "nb=4, nc=5, nr=2(nr < nb)"
  array_size = extractor%estimate_size(2)
  call assert_equal("estimate_size", array_size, 2)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=2", extractor%get_ptr(array, 2, 2), [(dble(i), i=1,2)])
  call end_array_tests()
  
end program 