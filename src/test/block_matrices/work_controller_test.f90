program work_controller_test
  use test_util
  use matrix_controller_m
  use work_controller_m
  implicit none
  class(matrix_controller_c), pointer :: controller
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)
  
  allocate(work_controller_c::controller)

  print *, "nb=3, nc=5, nr=5"
  array_size = controller%estimate_size(3, 5, 5)
  call assert_equal("estimate_size", array_size, 18)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 3, 5, 5, 2, 2), [(dble(i), i=1,1)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 3, 5, 5, 3, 2), [(dble(i), i=2,4)])
  call add_test("(i, j)=(4,2)", controller%get_ptr(array, 3, 5, 5, 4, 2), [(dble(i), i=5,5)])
  call add_test("(i, j)=(3,3)", controller%get_ptr(array, 3, 5, 5, 3, 3), [(dble(i), i=6,14)])
  call add_test("(i, j)=(4,3)", controller%get_ptr(array, 3, 5, 5, 4, 3), [(dble(i), i=15,17)])
  call add_test("(i, j)=(4,4)", controller%get_ptr(array, 3, 5, 5, 4, 4), [(dble(i), i=18,18)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=4"
  array_size = controller%estimate_size(3, 6, 4)
  call assert_equal("estimate_size", array_size, 13)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(3,3)", controller%get_ptr(array, 3, 6, 4, 3, 3), [(dble(i), i=1,9)])
  call add_test("(i, j)=(4,3)", controller%get_ptr(array, 3, 6, 4, 4, 3), [(dble(i), i=10,12)])
  call add_test("(i, j)=(4,4)", controller%get_ptr(array, 3, 6, 4, 4, 4), [(dble(i), i=13,13)])
  call end_array_tests()


  print *, "nb=3, nc=5, nr=5"
  array_size = controller%estimate_size(3, 7, 4)
  call assert_equal("estimate_size", array_size, 12)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(3,3)", controller%get_ptr(array, 3, 7, 4, 3, 3), [(dble(i), i=1,4)])
  call add_test("(i, j)=(4,3)", controller%get_ptr(array, 3, 7, 4, 4, 3), [(dble(i), i=5,8)])
  call add_test("(i, j)=(4,4)", controller%get_ptr(array, 3, 7, 4, 4, 4), [(dble(i), i=9,12)])
  call end_array_tests()

  print *, "nb=3, nc=5, nr=4"
  array_size = controller%estimate_size(3, 5, 4)
  call assert_equal("estimate_size", array_size, 13)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 3, 5, 4, 2, 2), [(dble(i), i=1,1)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 3, 5, 4, 3, 2), [(dble(i), i=2,4)])
  call add_test("(i, j)=(3,3)", controller%get_ptr(array, 3, 5, 4, 3, 3), [(dble(i), i=5,13)])
  call end_array_tests()

  print *, "nb=3, nc=3, nr=6"
  array_size = controller%estimate_size(3, 3, 6)
  call assert_equal("estimate_size", array_size, 27)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 3, 3, 6, 2, 2), [(dble(i), i=1,9)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 3, 3, 6, 3, 2), [(dble(i), i=10,18)])
  call add_test("(i, j)=(3,3)", controller%get_ptr(array, 3, 3, 6, 3, 3), [(dble(i), i=19,27)])
  call end_array_tests()


  print *, "nb=4, nc=2, nr=7"
  array_size = controller%estimate_size(4, 2, 7)
  call assert_equal("estimate_size", array_size, 35)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("(i, j)=(1,1)", controller%get_ptr(array, 4, 2, 7, 1, 1), [(dble(i), i=1,4)])
  call add_test("(i, j)=(2,1)", controller%get_ptr(array, 4, 2, 7, 2, 1), [(dble(i), i=5,12)])
  call add_test("(i, j)=(3,1)", controller%get_ptr(array, 4, 2, 7, 3, 1), [(dble(i), i=13,14)])
  call add_test("(i, j)=(2,2)", controller%get_ptr(array, 4, 2, 7, 2, 2), [(dble(i), i=15,30)])
  call add_test("(i, j)=(3,2)", controller%get_ptr(array, 4, 2, 7, 3, 2), [(dble(i), i=31,34)])
  call add_test("(i, j)=(3,3)", controller%get_ptr(array, 4, 2, 7, 3, 3), [(dble(i), i=35,35)])
  call end_array_tests()
  
end program work_controller_test