program supernode_matrix_extractor_test
  use test_util
  use matrix_extractor_m
  use border_matrix_extractor_m
  use node_data_m
  implicit none
  class(extractor_c), pointer :: extractor
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)

  allocate(border_extractor_c::extractor)
  extractor%node_data => create_node_data([5,6,7,5,3],[5,4,4,4,6],3)

  print *, "nb=3, nc=5, nr=5"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 21)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,1,2,2), [(dble(i), i=1,9)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,1,3,2), [(dble(i), i=10,18)])
  call add_test("(i, j)=(4,2)", extractor%get_ptr(array,1,4,2), [(dble(i), i=19,21)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=4"
  array_size = extractor%estimate_size(2)
  call assert_equal("estimate_size", array_size, 0)

  print *, "nb=3, nc=7, nr=4"
  array_size = extractor%estimate_size(3)
  call assert_equal("estimate_size", array_size, 15)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,3,3,3), [(dble(i), i=1,9)])
  call add_test("(i, j)=(4,3)", extractor%get_ptr(array,3,4,3), [(dble(i), i=10,15)])
  call end_array_tests()

  print *, "nb=3, nc=5, nr=4"
  array_size = extractor%estimate_size(4)
  call assert_equal("estimate_size", array_size, 18)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,4,2,2), [(dble(i), i=1,9)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,4,3,2), [(dble(i), i=10,18)])
  call end_array_tests()

  print *, "nb=3, nc=3, nr=6"
  array_size = extractor%estimate_size(5)
  call assert_equal("estimate_size", array_size, 0)

  extractor%node_data => create_node_data([2,5],[7,2],4)
  
  print *, "nb=4, nc=2, nr=7"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 36)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,1,1,1), [(dble(i), i=1,16)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,1,2,1), [(dble(i), i=17,32)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,1,3,1), [(dble(i), i=33,36)])
  call end_array_tests()

  print *, "nb=4, nc=5, nr=2(nr < nb)"
  array_size = extractor%estimate_size(2)
  call assert_equal("estimate_size", array_size, 9)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_tests("get_ptr")
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,2,2,2), [(dble(i), i=1,9)])
  call end_tests()
  
end program supernode_matrix_extractor_test


