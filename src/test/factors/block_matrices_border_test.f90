program block_matrices_border_test
  use block_matrices_m
  use matrix_extractor_m
  use border_matrix_extractor_m
  use test_util
  use node_data_m
  implicit none
  type(block_matrices_c), pointer :: block_matrices
  type(node_data_c), pointer :: node_data
  class(extractor_c), pointer :: extractor
  integer :: nb, i

  allocate(border_extractor_c::extractor)
  nb = 3
  node_data => create_node_data([1, 6, 4], [4, 5, 0], nb)
  block_matrices => create_block_matrices(node_data, extractor)

  block_matrices%get_ptr(1,1,1) = 0.0d0
  block_matrices%get_ptr(1,2,1) = 0.0d0
  
  block_matrices%get_ptr(1,1,1) = block_matrices%get_ptr(1,1,1) + 1.0d0
  block_matrices%get_ptr(1,2,1) = block_matrices%get_ptr(1,2,1) + 1.0d0
  
  call start_array_tests("double access & size:test1")
  call add_test("(node, i, j) = (1,1,1)", block_matrices%get_ptr(1,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (1,2,1)", block_matrices%get_ptr(1,2,1), [(1d0, i=1,6)])
  call end_array_tests()

  nb = 3
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  block_matrices => create_block_matrices(node_data, extractor)
  
  block_matrices%get_ptr(1,2,2) = 0d0
  block_matrices%get_ptr(1,3,2) = 0d0
  block_matrices%get_ptr(1,4,2) = 0d0
  block_matrices%get_ptr(3,3,3) = 0d0
  block_matrices%get_ptr(3,4,3) = 0d0
  block_matrices%get_ptr(4,2,2) = 0d0
  block_matrices%get_ptr(4,3,2) = 0d0

  block_matrices%get_ptr(1,2,2) = block_matrices%get_ptr(1,2,2) + 1d0
  block_matrices%get_ptr(1,3,2) = block_matrices%get_ptr(1,3,2) + 1d0
  block_matrices%get_ptr(1,4,2) = block_matrices%get_ptr(1,4,2) + 1d0
  block_matrices%get_ptr(3,3,3) = block_matrices%get_ptr(3,3,3) + 1d0
  block_matrices%get_ptr(3,4,3) = block_matrices%get_ptr(3,4,3) + 1d0
  block_matrices%get_ptr(4,2,2) = block_matrices%get_ptr(4,2,2) + 1d0
  block_matrices%get_ptr(4,3,2) = block_matrices%get_ptr(4,3,2) + 1d0

  call start_array_tests("double access & size:test2")
  call add_test("(1,2,2)", block_matrices%get_ptr(1,2,2), [(1d0, i=1,9)])
  call add_test("(1,3,2)", block_matrices%get_ptr(1,3,2), [(1d0, i=1,9)])
  call add_test("(1,4,2)", block_matrices%get_ptr(1,4,2), [(1d0, i=1,3)])
  call add_test("(3,3,3)", block_matrices%get_ptr(3,3,3), [(1d0, i=1,9)])
  call add_test("(3,4,3)", block_matrices%get_ptr(3,4,3), [(1d0, i=1,6)])
  call add_test("(4,2,2)", block_matrices%get_ptr(4,2,2), [(1d0, i=1,9)])
  call add_test("(4,3,2)", block_matrices%get_ptr(4,3,2), [(1d0, i=1,9)])
  call end_array_tests()

  nb = 3
  node_data => create_node_data([1,1,1,1,1,4], [2,3,3,3,3,0], nb)
  block_matrices => create_block_matrices(node_data, extractor)

  block_matrices%get_ptr(1,1,1) = 0.0d0
  block_matrices%get_ptr(2,1,1) = 0.0d0
  block_matrices%get_ptr(2,2,1) = 0.0d0
  block_matrices%get_ptr(3,1,1) = 0.0d0
  block_matrices%get_ptr(3,2,1) = 0.0d0
  block_matrices%get_ptr(4,1,1) = 0.0d0
  block_matrices%get_ptr(4,2,1) = 0.0d0
  block_matrices%get_ptr(5,1,1) = 0.0d0
  block_matrices%get_ptr(5,2,1) = 0.0d0
  
  
  block_matrices%get_ptr(1,1,1) = block_matrices%get_ptr(1,1,1) + 1d0
  block_matrices%get_ptr(2,1,1) = block_matrices%get_ptr(2,1,1) + 1d0
  block_matrices%get_ptr(2,2,1) = block_matrices%get_ptr(2,2,1) + 1d0
  block_matrices%get_ptr(3,1,1) = block_matrices%get_ptr(3,1,1) + 1d0
  block_matrices%get_ptr(3,2,1) = block_matrices%get_ptr(3,2,1) + 1d0
  block_matrices%get_ptr(4,1,1) = block_matrices%get_ptr(4,1,1) + 1d0
  block_matrices%get_ptr(4,2,1) = block_matrices%get_ptr(4,2,1) + 1d0
  block_matrices%get_ptr(5,1,1) = block_matrices%get_ptr(5,1,1) + 1d0
  block_matrices%get_ptr(5,2,1) = block_matrices%get_ptr(5,2,1) + 1d0
  
  call start_array_tests("double access & size:test1")
  call add_test("(node, i, j) = (1,1,1)", block_matrices%get_ptr(1,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,1,1)", block_matrices%get_ptr(2,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,2,1)", block_matrices%get_ptr(2,2,1), [(1d0, i=1,3)])
  call add_test("(node, i, j) = (3,1,1)", block_matrices%get_ptr(3,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (3,2,1)", block_matrices%get_ptr(3,2,1), [(1d0, i=1,3)])
  call add_test("(node, i, j) = (4,1,1)", block_matrices%get_ptr(4,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (4,2,1)", block_matrices%get_ptr(4,2,1), [(1d0, i=1,3)])
  call add_test("(node, i, j) = (5,1,1)", block_matrices%get_ptr(5,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (5,2,1)", block_matrices%get_ptr(5,2,1), [(1d0, i=1,3)])
  
  call end_array_tests()

end program