program block_arrays_work_test
  use block_arrays_m
  use array_extractor_m
  use work_array_extractor_m
  use test_util
  implicit none
  type(block_arrays_c), pointer :: block_arrays
  class(extractor_c), pointer :: extractor
  integer :: nb, i

  allocate(work_extractor_c::extractor)
  nb = 3
  block_arrays => create_block_arrays(nb, [1, 6, 4], [4, 5, 0], extractor)

  block_arrays%get_ptr(1,1) = 0.0d0
  block_arrays%get_ptr(1,2) = 0.0d0
  block_arrays%get_ptr(2,3) = 0.0d0
  block_arrays%get_ptr(2,4) = 0.0d0

  block_arrays%get_ptr(1,1) = block_arrays%get_ptr(1,1) + 1.0d0
  block_arrays%get_ptr(1,2) = block_arrays%get_ptr(1,2) + 1.0d0
  block_arrays%get_ptr(2,3) = block_arrays%get_ptr(2,3) + 1.0d0
  block_arrays%get_ptr(2,4) = block_arrays%get_ptr(2,4) + 1.0d0

  call start_array_tests("double access & size:test1")
  call add_test("(1,1)", block_arrays%get_ptr(1,1),[(1d0, i=1,2)])
  call add_test("(1,2)", block_arrays%get_ptr(1,2),[(1d0, i=1,2)])
  call add_test("(2,3)", block_arrays%get_ptr(2,3),[(1d0, i=1,3)])
  call add_test("(2,4)", block_arrays%get_ptr(2,4),[(1d0, i=1,2)])
  call end_array_tests()

  nb = 3
  block_arrays => create_block_arrays(nb, [5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], extractor)

  block_arrays%get_ptr(1,2) = 0d0
  block_arrays%get_ptr(1,3) = 0d0
  block_arrays%get_ptr(1,4) = 0d0
  block_arrays%get_ptr(2,3) = 0d0
  block_arrays%get_ptr(2,4) = 0d0
  block_arrays%get_ptr(3,3) = 0d0
  block_arrays%get_ptr(3,4) = 0d0
  block_arrays%get_ptr(4,2) = 0d0
  block_arrays%get_ptr(4,3) = 0d0
  block_arrays%get_ptr(5,2) = 0d0
  block_arrays%get_ptr(5,3) = 0d0
  

  block_arrays%get_ptr(1,2) = block_arrays%get_ptr(1,2) +1d0
  block_arrays%get_ptr(1,3) = block_arrays%get_ptr(1,3) +1d0
  block_arrays%get_ptr(1,4) = block_arrays%get_ptr(1,4) +1d0
  block_arrays%get_ptr(2,3) = block_arrays%get_ptr(2,3) +1d0
  block_arrays%get_ptr(2,4) = block_arrays%get_ptr(2,4) +1d0
  block_arrays%get_ptr(3,3) = block_arrays%get_ptr(3,3) +1d0
  block_arrays%get_ptr(3,4) = block_arrays%get_ptr(3,4) +1d0
  block_arrays%get_ptr(4,2) = block_arrays%get_ptr(4,2) +1d0
  block_arrays%get_ptr(4,3) = block_arrays%get_ptr(4,3) +1d0
  block_arrays%get_ptr(5,2) = block_arrays%get_ptr(5,2) + 1d0
  block_arrays%get_ptr(5,3) = block_arrays%get_ptr(5,3) + 1d0
  
  call start_array_tests("double access & size:test2")
  call add_test("(1,2)", block_arrays%get_ptr(1,2), [(1d0, i=1,1)])
  call add_test("(1,3)", block_arrays%get_ptr(1,3), [(1d0, i=1,3)])
  call add_test("(1,4)", block_arrays%get_ptr(1,4), [(1d0, i=1,1)])
  call add_test("(2,3)", block_arrays%get_ptr(2,3), [(1d0, i=1,3)])
  call add_test("(2,4)", block_arrays%get_ptr(2,4), [(1d0, i=1,1)])
  call add_test("(3,3)", block_arrays%get_ptr(3,3), [(1d0, i=1,2)])
  call add_test("(3,4)", block_arrays%get_ptr(3,4), [(1d0, i=1,2)])
  call add_test("(4,2)", block_arrays%get_ptr(4,2), [(1d0, i=1,1)])
  call add_test("(4,3)", block_arrays%get_ptr(4,3), [(1d0, i=1,3)])
  call add_test("(5,2)", block_arrays%get_ptr(5,2), [(1d0, i=1,3)])
  call add_test("(5,3)", block_arrays%get_ptr(5,3), [(1d0, i=1,3)])
  call end_array_tests()

end program