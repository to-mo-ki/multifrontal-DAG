program block_arrays_supernode_test
  use jagged_array_m
  use contiguous_sets_m
  use block_arrays_m
  use rh_controller_m
  use rh_supernode_controller_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs
  type(contiguous_sets_c), pointer :: node_sets
  type(block_arrays_c), pointer :: block_arrays
  class(rh_controller_c), pointer :: controller
  integer :: nb, i

  allocate(rh_supernode_controller_c::controller)
  nb = 3
  node_sets => create_contiguous_sets([1, 6, 4])
  ccs => create_jagged_array([4, 5, 0])
  block_arrays => create_block_arrays(nb, [1, 6, 4], [4, 5, 0], controller)

  block_arrays%get_ptr(1,1) = 0.0d0
  block_arrays%get_ptr(2,1) = 0.0d0
  block_arrays%get_ptr(2,2) = 0.0d0
  block_arrays%get_ptr(3,1) = 0.0d0
  block_arrays%get_ptr(3,2) = 0.0d0

  block_arrays%get_ptr(1,1) = block_arrays%get_ptr(1,1) + 1.0d0
  block_arrays%get_ptr(2,1) = block_arrays%get_ptr(2,1) + 1.0d0
  block_arrays%get_ptr(2,2) = block_arrays%get_ptr(2,2) + 1.0d0
  block_arrays%get_ptr(3,1) = block_arrays%get_ptr(3,1) + 1.0d0
  block_arrays%get_ptr(3,2) = block_arrays%get_ptr(3,2) + 1.0d0

  call start_array_tests("double access & size:test1")
  call add_test("(node, index) = (1,1)", block_arrays%get_ptr(1,1), [(1d0, i=1,1)])
  call add_test("(node, index) = (2,1)", block_arrays%get_ptr(2,1), [(1d0, i=1,3)])
  call add_test("(node, index) = (2,2)", block_arrays%get_ptr(2,2), [(1d0, i=1,3)])
  call add_test("(node, index) = (3,1)", block_arrays%get_ptr(3,1), [(1d0, i=1,3)])
  call add_test("(node, index) = (3,2)", block_arrays%get_ptr(3,2), [(1d0, i=1,1)])
  call end_array_tests()

  nb = 3
  node_sets => create_contiguous_sets([5, 6, 7, 5, 3, 6])
  ccs => create_jagged_array([5, 4, 4, 4, 6, 0])
  block_arrays => create_block_arrays(nb, [5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], controller)

  block_arrays%get_ptr(1,1) = 0d0
  block_arrays%get_ptr(1,2) = 0d0
  block_arrays%get_ptr(2,1) = 0d0
  block_arrays%get_ptr(2,2) = 0d0
  block_arrays%get_ptr(3,1) = 0d0
  block_arrays%get_ptr(3,2) = 0d0
  block_arrays%get_ptr(3,3) = 0d0
  block_arrays%get_ptr(4,1) = 0d0
  block_arrays%get_ptr(4,2) = 0d0
  block_arrays%get_ptr(5,1) = 0d0
  block_arrays%get_ptr(6,1) = 0d0
  block_arrays%get_ptr(6,2) = 0d0
  
  block_arrays%get_ptr(1,1) = block_arrays%get_ptr(1,1) + 1d0
  block_arrays%get_ptr(1,2) = block_arrays%get_ptr(1,2) + 1d0
  block_arrays%get_ptr(2,1) = block_arrays%get_ptr(2,1) + 1d0
  block_arrays%get_ptr(2,2) = block_arrays%get_ptr(2,2) + 1d0
  block_arrays%get_ptr(3,1) = block_arrays%get_ptr(3,1) + 1d0
  block_arrays%get_ptr(3,2) = block_arrays%get_ptr(3,2) + 1d0
  block_arrays%get_ptr(3,3) = block_arrays%get_ptr(3,3) + 1d0
  block_arrays%get_ptr(4,1) = block_arrays%get_ptr(4,1) + 1d0
  block_arrays%get_ptr(4,2) = block_arrays%get_ptr(4,2) + 1d0
  block_arrays%get_ptr(5,1) = block_arrays%get_ptr(5,1) + 1d0
  block_arrays%get_ptr(6,1) = block_arrays%get_ptr(6,1) + 1d0
  block_arrays%get_ptr(6,2) = block_arrays%get_ptr(6,2) + 1d0
  
  call start_array_tests("double access & size:test2")
  call add_test("(1,1)", block_arrays%get_ptr(1,1), [(1d0, i=1,3)])
  call add_test("(1,2)", block_arrays%get_ptr(1,2), [(1d0, i=1,2)])
  call add_test("(2,1)", block_arrays%get_ptr(2,1), [(1d0, i=1,3)])
  call add_test("(2,2)", block_arrays%get_ptr(2,2), [(1d0, i=1,3)])
  call add_test("(3,1)", block_arrays%get_ptr(3,1), [(1d0, i=1,3)])
  call add_test("(3,2)", block_arrays%get_ptr(3,2), [(1d0, i=1,3)])
  call add_test("(3,3)", block_arrays%get_ptr(3,3), [(1d0, i=1,1)])
  call add_test("(4,1)", block_arrays%get_ptr(4,1), [(1d0, i=1,3)])
  call add_test("(4,2)", block_arrays%get_ptr(4,2), [(1d0, i=1,2)])
  call add_test("(5,1)", block_arrays%get_ptr(5,1), [(1d0, i=1,3)])
  call add_test("(6,1)", block_arrays%get_ptr(6,1), [(1d0, i=1,3)])
  call add_test("(6,2)", block_arrays%get_ptr(6,2), [(1d0, i=1,3)])
  call end_array_tests()

end program