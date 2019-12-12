program block_arrays_border_test
  use block_arrays_m
  use extractor_m
  use border_array_extractor_m
  use test_util
  implicit none
  type(block_arrays_c), pointer :: block_arrays
  class(extractor_c), pointer :: controller
  integer :: nb, i

  allocate(border_extractor_c::controller)
  nb = 3
  block_arrays => create_block_arrays(nb, [1,6,4], [4,5,0], controller)

  block_arrays%get_ptr(1,1) = 0.0d0
  block_arrays%get_ptr(1,1) = block_arrays%get_ptr(1,1) + 1.0d0
  call start_array_tests("double access & size:test1")
  call add_test("(node, index) = (1,1)", block_arrays%get_ptr(1,1), [(1d0, i=1,3)])
  call end_array_tests()

  nb = 3
  block_arrays => create_block_arrays(nb, [5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], controller)

  block_arrays%get_ptr(1,2) = 0d0
  block_arrays%get_ptr(3,3) = 0d0
  block_arrays%get_ptr(4,2) = 0d0
  
  block_arrays%get_ptr(1,2) = block_arrays%get_ptr(1,2) + 1d0
  block_arrays%get_ptr(3,3) = block_arrays%get_ptr(3,3) + 1d0
  block_arrays%get_ptr(4,2) = block_arrays%get_ptr(4,2) + 1d0
  
  call start_array_tests("double access & size:test2")
  call add_test("(1,2)", block_arrays%get_ptr(1,2), [(1d0, i=1,3)])
  call add_test("(3,3)", block_arrays%get_ptr(3,3), [(1d0, i=1,3)])
  call add_test("(4,2)", block_arrays%get_ptr(4,2), [(1d0, i=1,3)])
  call end_array_tests()

end program