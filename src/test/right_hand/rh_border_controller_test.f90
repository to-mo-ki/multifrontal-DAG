program rh_border_controller_test
  use rh_controller_m
  use rh_border_controller_m
  use test_util
  implicit none
  class(rh_controller_c), pointer :: controller
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)
  
  allocate(rh_border_controller_c::controller)
  
  print *, "nb=3, nc=5, nr=5"
  array_size = controller%estimate_size(3, 5, 5)
  call assert_equal("estimate_size", array_size, 3)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=2", controller%get_ptr(array, 3, 5, 5, 2), [(dble(i), i=1,3)])
  call end_array_tests()

  print *, "nb=3, nc=6, nr=4"
  array_size = controller%estimate_size(3, 6, 4)
  call assert_equal("estimate_size", array_size, 0)

  print *, "nb=3, nc=7, nr=4"
  array_size = controller%estimate_size(3, 7, 4)
  call assert_equal("estimate_size", array_size, 3)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=3", controller%get_ptr(array, 3, 7, 4, 3), [(dble(i), i=1,3)])
  call end_array_tests()

  print *, "nb=3, nc=5, nr=4"
  array_size = controller%estimate_size(3, 5, 4)
  call assert_equal("estimate_size", array_size, 3)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=2", controller%get_ptr(array, 3, 5, 4, 2), [(dble(i), i=1,3)])
  call end_array_tests()

  print *, "nb=3, nc=3, nr=6"
  array_size = controller%estimate_size(3, 3, 6)
  call assert_equal("estimate_size", array_size, 0)
  
  print *, "nb=3, nc=6, nr=0"
  array_size = controller%estimate_size(3, 6, 0)
  call assert_equal("estimate_size", array_size, 0)

  print *, "nb=4, nc=2, nr=5(nc < nb)"
  array_size = controller%estimate_size(4, 2, 5)
  call assert_equal("estimate_size", array_size, 4)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=1", controller%get_ptr(array, 4, 2, 5, 1), [(dble(i), i=1,4)])
  call end_array_tests()

  print *, "nb=4, nc=5, nr=2(nr < nb)"
  array_size = controller%estimate_size(4, 5, 2)
  call assert_equal("estimate_size", array_size, 3)
  allocate(array(array_size))
  array = [(dble(i), i=1,array_size)]
  call start_array_tests("get_ptr")
  call add_test("index=2", controller%get_ptr(array, 4, 5, 2, 2), [(dble(i), i=1,3)])
  call end_array_tests()
  
end program 