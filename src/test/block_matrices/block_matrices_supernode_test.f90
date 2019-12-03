program block_matrices_supernode_test
  use jagged_array_m
  use contiguous_sets_m
  use block_matrices_m
  use matrix_controller_m
  use supernode_controller_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs
  type(contiguous_sets_c), pointer :: node_sets
  type(block_matrices_c), pointer :: block_matrices
  class(matrix_controller_c), pointer :: controller
  integer :: nb, i

  allocate(supernode_controller_c::controller)
  nb = 3
  node_sets => create_contiguous_sets([1, 6, 4])
  ccs => create_jagged_array([4, 5, 0])
  block_matrices => create_block_matrices(nb, node_sets, ccs, controller)

  block_matrices%get_ptr(1,1,1) = 0.0d0
  block_matrices%get_ptr(1,2,1) = 0.0d0
  block_matrices%get_ptr(2,1,1) = 0.0d0
  block_matrices%get_ptr(2,2,1) = 0.0d0
  block_matrices%get_ptr(2,3,1) = 0.0d0
  block_matrices%get_ptr(2,4,1) = 0.0d0
  block_matrices%get_ptr(2,2,2) = 0.0d0
  block_matrices%get_ptr(2,3,2) = 0.0d0
  block_matrices%get_ptr(2,4,2) = 0.0d0
  block_matrices%get_ptr(3,1,1) = 0.0d0
  block_matrices%get_ptr(3,2,1) = 0.0d0
  block_matrices%get_ptr(3,2,2) = 0.0d0

  block_matrices%get_ptr(1,1,1) = block_matrices%get_ptr(1,1,1) + 1.0d0
  block_matrices%get_ptr(1,2,1) = block_matrices%get_ptr(1,2,1) + 1.0d0
  block_matrices%get_ptr(2,1,1) = block_matrices%get_ptr(2,1,1) + 1.0d0
  block_matrices%get_ptr(2,2,1) = block_matrices%get_ptr(2,2,1) + 1.0d0
  block_matrices%get_ptr(2,3,1) = block_matrices%get_ptr(2,3,1) + 1.0d0
  block_matrices%get_ptr(2,4,1) = block_matrices%get_ptr(2,4,1) + 1.0d0
  block_matrices%get_ptr(2,2,2) = block_matrices%get_ptr(2,2,2) + 1.0d0
  block_matrices%get_ptr(2,3,2) = block_matrices%get_ptr(2,3,2) + 1.0d0
  block_matrices%get_ptr(2,4,2) = block_matrices%get_ptr(2,4,2) + 1.0d0
  block_matrices%get_ptr(3,1,1) = block_matrices%get_ptr(3,1,1) + 1.0d0
  block_matrices%get_ptr(3,2,1) = block_matrices%get_ptr(3,2,1) + 1.0d0
  block_matrices%get_ptr(3,2,2) = block_matrices%get_ptr(3,2,2) + 1.0d0

  call start_array_tests("double access & size:test1")
  call add_test("(node, i, j) = (1,1,1)", block_matrices%get_ptr(1,1,1), [(1d0, i=1,3)])
  call add_test("(node, i, j) = (1,2,1)", block_matrices%get_ptr(1,2,1), [(1d0, i=1,2)])
  call add_test("(node, i, j) = (2,1,1)", block_matrices%get_ptr(2,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,2,1)", block_matrices%get_ptr(2,2,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,3,1)", block_matrices%get_ptr(2,3,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,1)", block_matrices%get_ptr(2,4,1), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (2,2,2)", block_matrices%get_ptr(2,2,2), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,3,2)", block_matrices%get_ptr(2,3,2), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,2)", block_matrices%get_ptr(2,4,2), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (3,1,1)", block_matrices%get_ptr(3,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (3,2,1)", block_matrices%get_ptr(3,2,1), [(1d0, i=1,3)])
  call add_test("(node, i, j) = (3,2,2)", block_matrices%get_ptr(3,2,2), [(1d0, i=1,1)])
  call end_array_tests()

  nb = 3
  node_sets => create_contiguous_sets([5, 6, 7, 5, 3, 6])
  ccs => create_jagged_array([5, 4, 4, 4, 6, 0])
  block_matrices => create_block_matrices(nb, node_sets, ccs, controller)

  block_matrices%get_ptr(1,1,1) = 0d0
  block_matrices%get_ptr(1,2,1) = 0d0
  block_matrices%get_ptr(1,3,1) = 0d0
  block_matrices%get_ptr(1,4,1) = 0d0
  block_matrices%get_ptr(1,2,2) = 0d0
  block_matrices%get_ptr(1,3,2) = 0d0
  block_matrices%get_ptr(1,4,2) = 0d0
  block_matrices%get_ptr(2,1,1) = 0d0
  block_matrices%get_ptr(2,2,1) = 0d0
  block_matrices%get_ptr(2,3,1) = 0d0
  block_matrices%get_ptr(2,4,1) = 0d0
  block_matrices%get_ptr(2,2,2) = 0d0
  block_matrices%get_ptr(2,3,2) = 0d0
  block_matrices%get_ptr(2,4,2) = 0d0
  block_matrices%get_ptr(3,1,1) = 0d0
  block_matrices%get_ptr(3,2,1) = 0d0
  block_matrices%get_ptr(3,3,1) = 0d0
  block_matrices%get_ptr(3,4,1) = 0d0
  block_matrices%get_ptr(3,2,2) = 0d0
  block_matrices%get_ptr(3,3,2) = 0d0
  block_matrices%get_ptr(3,4,2) = 0d0
  block_matrices%get_ptr(3,3,3) = 0d0
  block_matrices%get_ptr(3,4,3) = 0d0
  block_matrices%get_ptr(4,1,1) = 0d0
  block_matrices%get_ptr(4,2,1) = 0d0
  block_matrices%get_ptr(4,3,1) = 0d0
  block_matrices%get_ptr(4,2,2) = 0d0
  block_matrices%get_ptr(4,3,2) = 0d0
  block_matrices%get_ptr(5,1,1) = 0d0
  block_matrices%get_ptr(5,2,1) = 0d0
  block_matrices%get_ptr(5,3,1) = 0d0
  block_matrices%get_ptr(6,1,1) = 0d0
  block_matrices%get_ptr(6,2,1) = 0d0
  block_matrices%get_ptr(6,2,2) = 0d0

  block_matrices%get_ptr(1,1,1) = block_matrices%get_ptr(1,1,1) + 1d0
  block_matrices%get_ptr(1,2,1) = block_matrices%get_ptr(1,2,1) + 1d0
  block_matrices%get_ptr(1,3,1) = block_matrices%get_ptr(1,3,1) + 1d0
  block_matrices%get_ptr(1,4,1) = block_matrices%get_ptr(1,4,1) + 1d0
  block_matrices%get_ptr(1,2,2) = block_matrices%get_ptr(1,2,2) + 1d0
  block_matrices%get_ptr(1,3,2) = block_matrices%get_ptr(1,3,2) + 1d0
  block_matrices%get_ptr(1,4,2) = block_matrices%get_ptr(1,4,2) + 1d0
  block_matrices%get_ptr(2,1,1) = block_matrices%get_ptr(2,1,1) + 1d0
  block_matrices%get_ptr(2,2,1) = block_matrices%get_ptr(2,2,1) + 1d0
  block_matrices%get_ptr(2,3,1) = block_matrices%get_ptr(2,3,1) + 1d0
  block_matrices%get_ptr(2,4,1) = block_matrices%get_ptr(2,4,1) + 1d0
  block_matrices%get_ptr(2,2,2) = block_matrices%get_ptr(2,2,2) + 1d0
  block_matrices%get_ptr(2,3,2) = block_matrices%get_ptr(2,3,2) + 1d0
  block_matrices%get_ptr(2,4,2) = block_matrices%get_ptr(2,4,2) + 1d0
  block_matrices%get_ptr(3,1,1) = block_matrices%get_ptr(3,1,1) + 1d0
  block_matrices%get_ptr(3,2,1) = block_matrices%get_ptr(3,2,1) + 1d0
  block_matrices%get_ptr(3,3,1) = block_matrices%get_ptr(3,3,1) + 1d0
  block_matrices%get_ptr(3,4,1) = block_matrices%get_ptr(3,4,1) + 1d0
  block_matrices%get_ptr(3,2,2) = block_matrices%get_ptr(3,2,2) + 1d0
  block_matrices%get_ptr(3,3,2) = block_matrices%get_ptr(3,3,2) + 1d0
  block_matrices%get_ptr(3,4,2) = block_matrices%get_ptr(3,4,2) + 1d0
  block_matrices%get_ptr(3,3,3) = block_matrices%get_ptr(3,3,3) + 1d0
  block_matrices%get_ptr(3,4,3) = block_matrices%get_ptr(3,4,3) + 1d0
  block_matrices%get_ptr(4,1,1) = block_matrices%get_ptr(4,1,1) + 1d0
  block_matrices%get_ptr(4,2,1) = block_matrices%get_ptr(4,2,1) + 1d0
  block_matrices%get_ptr(4,3,1) = block_matrices%get_ptr(4,3,1) + 1d0
  block_matrices%get_ptr(4,2,2) = block_matrices%get_ptr(4,2,2) + 1d0
  block_matrices%get_ptr(4,3,2) = block_matrices%get_ptr(4,3,2) + 1d0
  block_matrices%get_ptr(5,1,1) = block_matrices%get_ptr(5,1,1) + 1d0
  block_matrices%get_ptr(5,2,1) = block_matrices%get_ptr(5,2,1) + 1d0
  block_matrices%get_ptr(5,3,1) = block_matrices%get_ptr(5,3,1) + 1d0
  block_matrices%get_ptr(6,1,1) = block_matrices%get_ptr(6,1,1) + 1d0
  block_matrices%get_ptr(6,2,1) = block_matrices%get_ptr(6,2,1) + 1d0
  block_matrices%get_ptr(6,2,2) = block_matrices%get_ptr(6,2,2) + 1d0

  call start_array_tests("double access & size:test2")
  call add_test("(1,1,1)", block_matrices%get_ptr(1,1,1), [(1d0, i=1,9)])
  call add_test("(1,2,1)", block_matrices%get_ptr(1,2,1), [(1d0, i=1,9)])
  call add_test("(1,3,1)", block_matrices%get_ptr(1,3,1), [(1d0, i=1,9)])
  call add_test("(1,4,1)", block_matrices%get_ptr(1,4,1), [(1d0, i=1,3)])
  call add_test("(1,2,2)", block_matrices%get_ptr(1,2,2), [(1d0, i=1,6)])
  call add_test("(1,3,2)", block_matrices%get_ptr(1,3,2), [(1d0, i=1,6)])
  call add_test("(1,4,2)", block_matrices%get_ptr(1,4,2), [(1d0, i=1,2)])
  call add_test("(2,1,1)", block_matrices%get_ptr(2,1,1), [(1d0, i=1,9)])
  call add_test("(2,2,1)", block_matrices%get_ptr(2,2,1), [(1d0, i=1,9)])
  call add_test("(2,3,1)", block_matrices%get_ptr(2,3,1), [(1d0, i=1,9)])
  call add_test("(2,4,1)", block_matrices%get_ptr(2,4,1), [(1d0, i=1,3)])
  call add_test("(2,2,2)", block_matrices%get_ptr(2,2,2), [(1d0, i=1,9)])
  call add_test("(2,3,2)", block_matrices%get_ptr(2,3,2), [(1d0, i=1,9)])
  call add_test("(2,4,2)", block_matrices%get_ptr(2,4,2), [(1d0, i=1,3)])
  call add_test("(3,1,1)", block_matrices%get_ptr(3,1,1), [(1d0, i=1,9)])
  call add_test("(3,2,1)", block_matrices%get_ptr(3,2,1), [(1d0, i=1,9)])
  call add_test("(3,3,1)", block_matrices%get_ptr(3,3,1), [(1d0, i=1,9)])
  call add_test("(3,4,1)", block_matrices%get_ptr(3,4,1), [(1d0, i=1,6)])
  call add_test("(3,2,2)", block_matrices%get_ptr(3,2,2), [(1d0, i=1,9)])
  call add_test("(3,3,2)", block_matrices%get_ptr(3,3,2), [(1d0, i=1,9)])
  call add_test("(3,4,2)", block_matrices%get_ptr(3,4,2), [(1d0, i=1,6)])
  call add_test("(3,3,3)", block_matrices%get_ptr(3,3,3), [(1d0, i=1,3)])
  call add_test("(3,4,3)", block_matrices%get_ptr(3,4,3), [(1d0, i=1,2)])
  call add_test("(4,1,1)", block_matrices%get_ptr(4,1,1), [(1d0, i=1,9)])
  call add_test("(4,2,1)", block_matrices%get_ptr(4,2,1), [(1d0, i=1,9)])
  call add_test("(4,3,1)", block_matrices%get_ptr(4,3,1), [(1d0, i=1,9)])
  call add_test("(4,2,2)", block_matrices%get_ptr(4,2,2), [(1d0, i=1,6)])
  call add_test("(4,3,2)", block_matrices%get_ptr(4,3,2), [(1d0, i=1,6)])
  call add_test("(5,1,1)", block_matrices%get_ptr(5,1,1), [(1d0, i=1,9)])
  call add_test("(5,2,1)", block_matrices%get_ptr(5,2,1), [(1d0, i=1,9)])
  call add_test("(5,3,1)", block_matrices%get_ptr(5,3,1), [(1d0, i=1,9)])
  call add_test("(6,1,1)", block_matrices%get_ptr(6,1,1), [(1d0, i=1,9)])
  call add_test("(6,2,1)", block_matrices%get_ptr(6,2,1), [(1d0, i=1,9)])
  call add_test("(6,2,2)", block_matrices%get_ptr(6,2,2), [(1d0, i=1,9)])
  call end_array_tests()

end program