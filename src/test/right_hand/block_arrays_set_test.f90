program block_arrays_supernode_test
  use block_arrays_m
  use extractor_m
  use supernode_array_extractor_m
  use test_util
  implicit none
  type(block_arrays_c), pointer :: block_arrays
  class(extractor_c), pointer :: controller
  double precision, pointer, contiguous :: val(:)
  integer :: nb, i

  allocate(supernode_extractor_c::controller)
  allocate(val, source=[(dble(i),i=1,11)])
  nb = 3
  block_arrays => create_block_arrays(nb, [1, 6, 4], [4, 5, 0], controller)

  call block_arrays%set_val(val)
  call start_array_tests("set")
  call add_test("(node, index) = (1,1)", block_arrays%get_ptr(1,1), [1d0])
  call add_test("(node, index) = (2,1)", block_arrays%get_ptr(2,1), [2d0,3d0,4d0])
  call add_test("(node, index) = (2,2)", block_arrays%get_ptr(2,2), [5d0,6d0,7d0])
  call add_test("(node, index) = (3,1)", block_arrays%get_ptr(3,1), [8d0,9d0,10d0])
  call add_test("(node, index) = (3,2)", block_arrays%get_ptr(3,2), [11d0])
  call end_array_tests()

end program