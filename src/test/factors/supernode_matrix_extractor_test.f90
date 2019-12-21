program supernode_matrix_extractor_test
  use test_util
  use matrix_extractor_m
  use supernode_matrix_extractor_m
  use node_data_m
  implicit none
  class(extractor_c), pointer :: extractor
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)

  allocate(supernode_extractor_c::extractor)
  extractor%node_data => create_node_data([5,6,7,5,3,6,1],[5,4,4,4,6,0,4],3)

  print *, "nb=3, nc=5, nr=5"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 44)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,1,1,1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,1,2,1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,1,3,1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(4,1)", extractor%get_ptr(array,1,4,1), [(dble(i), i=28,30)])
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,1,2,2), [(dble(i), i=31,36)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,1,3,2), [(dble(i), i=37,42)])
  call add_test("(i, j)=(4,2)", extractor%get_ptr(array,1,4,2), [(dble(i), i=43,44)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=4"
  array_size = extractor%estimate_size(2)
  call assert_equal("estimate_size", array_size, 51)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,2,1,1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,2,2,1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,2,3,1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(4,1)", extractor%get_ptr(array,2,4,1), [(dble(i), i=28,30)])
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,2,2,2), [(dble(i), i=31,39)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,2,3,2), [(dble(i), i=40,48)])
  call add_test("(i, j)=(4,2)", extractor%get_ptr(array,2,4,2), [(dble(i), i=49,51)])
  call end_array_tests()

  print *, "nb=3, nc=7, nr=4"
  array_size = extractor%estimate_size(3)
  call assert_equal("estimate_size", array_size, 62)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,3,1,1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,3,2,1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,3,3,1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(4,1)", extractor%get_ptr(array,3,4,1), [(dble(i), i=28,33)])
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,3,2,2), [(dble(i), i=34,42)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,3,3,2), [(dble(i), i=43,51)])
  call add_test("(i, j)=(4,2)", extractor%get_ptr(array,3,4,2), [(dble(i), i=52,57)])
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,3,3,3), [(dble(i), i=58,60)])
  call add_test("(i, j)=(4,3)", extractor%get_ptr(array,3,4,3), [(dble(i), i=61,62)])
  call end_array_tests()

  print *, "nb=3, nc=5, nr=4"
  array_size = extractor%estimate_size(4)
  call assert_equal("estimate_size", array_size, 39)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,4,1,1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,4,2,1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,4,3,1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,4,2,2), [(dble(i), i=28,33)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,4,3,2), [(dble(i), i=34,39)])
  call end_array_tests()

  print *, "nb=3, nc=3, nr=6"
  array_size = extractor%estimate_size(5)
  call assert_equal("estimate_size", array_size, 27)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,5,1,1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,5,2,1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,5,3,1), [(dble(i), i=19,27)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=0"
  array_size = extractor%estimate_size(6)
  call assert_equal("estimate_size", array_size, 27)

  print *, "nb=3, nc=1, nr=4"
  array_size = extractor%estimate_size(7)
  call assert_equal("estimate_size", array_size, 5)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,7,1,1), [(dble(i), i=1,3)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,7,2,1), [(dble(i), i=4,5)])
  call end_array_tests()

  extractor%node_data => create_node_data([2],[7],4)

  print *, "nb=4, nc=2, nr=7"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 18)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,1,1,1), [(dble(i), i=1,8)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,1,2,1), [(dble(i), i=9,16)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,1,3,1), [(dble(i), i=17,18)])
  call end_array_tests()

end program supernode_matrix_extractor_test


