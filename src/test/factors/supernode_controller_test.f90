program supernode_controller_test
  use test_util
  use matrix_controller_m
  use supernode_controller_m
  implicit none
  class(matrix_controller_c), pointer :: controller
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)

  allocate(supernode_controller_c::controller)
  
  print *, "nb=3, nc=5, nr=5"
  array_size = controller%estimate_size(3, 5, 5)
  call assert_equal("estimate_size", array_size, 44)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 3, 5, 5, 1, 1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 3, 5, 5, 2, 1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", controller%get_ptr(array, 3, 5, 5, 3, 1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(4,1)", controller%get_ptr(array, 3, 5, 5, 4, 1), [(dble(i), i=28,30)])
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 3, 5, 5, 2, 2), [(dble(i), i=31,36)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 3, 5, 5, 3, 2), [(dble(i), i=37,42)])
  call add_test("(i, j)=(4,2)", controller%get_ptr(array, 3, 5, 5, 4, 2), [(dble(i), i=43,44)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=4"
  array_size = controller%estimate_size(3, 6, 4)
  call assert_equal("estimate_size", array_size, 51)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 3, 6, 4, 1, 1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 3, 6, 4, 2, 1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", controller%get_ptr(array, 3, 6, 4, 3, 1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(4,1)", controller%get_ptr(array, 3, 6, 4, 4, 1), [(dble(i), i=28,30)])
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 3, 6, 4, 2, 2), [(dble(i), i=31,39)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 3, 6, 4, 3, 2), [(dble(i), i=40,48)])
  call add_test("(i, j)=(4,2)", controller%get_ptr(array, 3, 6, 4, 4, 2), [(dble(i), i=49,51)])
  call end_array_tests()


  print *, "nb=3, nc=7, nr=4"
  array_size = controller%estimate_size(3, 7, 4)
  call assert_equal("estimate_size", array_size, 62)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 3, 7, 4, 1, 1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 3, 7, 4, 2, 1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", controller%get_ptr(array, 3, 7, 4, 3, 1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(4,1)", controller%get_ptr(array, 3, 7, 4, 4, 1), [(dble(i), i=28,33)])
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 3, 7, 4, 2, 2), [(dble(i), i=34,42)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 3, 7, 4, 3, 2), [(dble(i), i=43,51)])
  call add_test("(i, j)=(4,2)", controller%get_ptr(array, 3, 7, 4, 4, 2), [(dble(i), i=52,57)])
  call add_test("(i, j)=(3,3)", controller%get_ptr(array, 3, 7, 4, 3, 3), [(dble(i), i=58,60)])
  call add_test("(i, j)=(4,3)", controller%get_ptr(array, 3, 7, 4, 4, 3), [(dble(i), i=61,62)])
  call end_array_tests()

  print *, "nb=3, nc=5, nr=4"
  array_size = controller%estimate_size(3, 5, 4)
  call assert_equal("estimate_size", array_size, 39)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 3, 5, 4, 1, 1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 3, 5, 4, 2, 1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", controller%get_ptr(array, 3, 5, 4, 3, 1), [(dble(i), i=19,27)])
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 3, 5, 4, 2, 2), [(dble(i), i=28,33)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 3, 5, 4, 3, 2), [(dble(i), i=34,39)])
  call end_array_tests()

  print *, "nb=3, nc=3, nr=6"
  array_size = controller%estimate_size(3, 3, 6)
  call assert_equal("estimate_size", array_size, 27)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 3, 3, 6, 1, 1), [(dble(i), i=1,9)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 3, 3, 6, 2, 1), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,1)", controller%get_ptr(array, 3, 3, 6, 3, 1), [(dble(i), i=19,27)])
  call end_array_tests()

  print *, "nb=4, nc=2, nr=7"
  array_size = controller%estimate_size(4, 2, 7)
  call assert_equal("estimate_size", array_size, 18)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 4, 2, 7, 1, 1), [(dble(i), i=1,8)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 4, 2, 7, 2, 1), [(dble(i), i=9,16)])
  call add_test("(i, j)=(3,1)", controller%get_ptr(array, 4, 2, 7, 3, 1), [(dble(i), i=17,18)])
  call end_array_tests()

  print *, "nb=3, nc=1, nr=4"
  array_size = controller%estimate_size(3, 1, 4)
  call assert_equal("estimate_size", array_size, 5)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 3, 1, 4, 1, 1), [(dble(i), i=1,3)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 3, 1, 4, 2, 1), [(dble(i), i=4,5)])
  call end_array_tests()
  
end program supernode_controller_test


