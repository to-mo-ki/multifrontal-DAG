program work_matrix_extractor_test
  use test_util
  use matrix_extractor_m
  use work_matrix_extractor_m
  use node_data_m
  implicit none
  class(extractor_c), pointer :: extractor
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)
  
  allocate(work_extractor_c::extractor)
  extractor%node_data => create_node_data([5,6,7,5,3,6],[5,4,4,4,6,0],3)

  print *, "nb=3, nc=5, nr=5"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 18)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,1,2,2), [(dble(i), i=1,1)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,1,3,2), [(dble(i), i=2,4)])
  call add_test("(i, j)=(4,2)", extractor%get_ptr(array,1,4,2), [(dble(i), i=5,5)])
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,1,3,3), [(dble(i), i=6,14)])
  call add_test("(i, j)=(4,3)", extractor%get_ptr(array,1,4,3), [(dble(i), i=15,17)])
  call add_test("(i, j)=(4,4)", extractor%get_ptr(array,1,4,4), [(dble(i), i=18,18)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=4"
  array_size = extractor%estimate_size(2)
  call assert_equal("estimate_size", array_size, 13)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,2,3,3), [(dble(i), i=1,9)])
  call add_test("(i, j)=(4,3)", extractor%get_ptr(array,2,4,3), [(dble(i), i=10,12)])
  call add_test("(i, j)=(4,4)", extractor%get_ptr(array,2,4,4), [(dble(i), i=13,13)])
  call end_array_tests()


  print *, "nb=3, nc=7, nr=4"
  array_size = extractor%estimate_size(3)
  call assert_equal("estimate_size", array_size, 12)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,3,3,3), [(dble(i), i=1,4)])
  call add_test("(i, j)=(4,3)", extractor%get_ptr(array,3,4,3), [(dble(i), i=5,8)])
  call add_test("(i, j)=(4,4)", extractor%get_ptr(array,3,4,4), [(dble(i), i=9,12)])
  call end_array_tests()

  print *, "nb=3, nc=5, nr=4"
  array_size = extractor%estimate_size(4)
  call assert_equal("estimate_size", array_size, 13)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,4,2,2), [(dble(i), i=1,1)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,4,3,2), [(dble(i), i=2,4)])
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,4,3,3), [(dble(i), i=5,13)])
  call end_array_tests()

  print *, "nb=3, nc=3, nr=6"
  array_size = extractor%estimate_size(5)
  call assert_equal("estimate_size", array_size, 27)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,5,2,2), [(dble(i), i=1,9)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,5,3,2), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,5,3,3), [(dble(i), i=19,27)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=0"
  array_size = extractor%estimate_size(6)
  call assert_equal("estimate_size", array_size, 0)

  extractor%node_data => create_node_data([2],[7],4)
  print *, "nb=4, nc=2, nr=7"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 35)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,1,1,1), [(dble(i), i=1,4)])
  call add_test("(i, j)=(2,1)", extractor%get_ptr(array,1,2,1), [(dble(i), i=5,12)])
  call add_test("(i, j)=(3,1)", extractor%get_ptr(array,1,3,1), [(dble(i), i=13,14)])
  call add_test("(i, j)=(2,2)", extractor%get_ptr(array,1,2,2), [(dble(i), i=15,30)])
  call add_test("(i, j)=(3,2)", extractor%get_ptr(array,1,3,2), [(dble(i), i=31,34)])
  call add_test("(i, j)=(3,3)", extractor%get_ptr(array,1,3,3), [(dble(i), i=35,35)])
  call end_array_tests()
  
  extractor%node_data => create_node_data([1],[7],10)
  print *, "nb=10, nc=1, nr=7"
  array_size = extractor%estimate_size(1)
  call assert_equal("estimate_size", array_size, 49)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", extractor%get_ptr(array,1,1,1), [(dble(i), i=1,49)])
  call end_array_tests()
  
end program work_matrix_extractor_test