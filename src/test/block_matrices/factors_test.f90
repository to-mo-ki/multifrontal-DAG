program factors_test
  use contiguous_sets_m
  use jagged_array_m
  use factors_m
  use test_util
  use node_data_m
  implicit none

  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: ccs
  type(node_data_c), pointer :: node_data
  integer :: nb, ssize, wsize, i

  nb = 3
  node_sets => create_contiguous_sets([1, 6, 4])
  ccs => create_jagged_array([4, 5, 0])
  node_data => create_node_data([1,6,4], [4,5,0], nb)
  factors => create_factors(node_data, nb)

  factors%get_supernode_ptr(1,1,1) = 0.0d0
  factors%get_supernode_ptr(1,2,1) = 0.0d0
  factors%get_supernode_ptr(2,1,1) = 0.0d0
  factors%get_supernode_ptr(2,2,1) = 0.0d0
  factors%get_supernode_ptr(2,3,1) = 0.0d0
  factors%get_supernode_ptr(2,4,1) = 0.0d0
  factors%get_supernode_ptr(2,2,2) = 0.0d0
  factors%get_supernode_ptr(2,3,2) = 0.0d0
  factors%get_supernode_ptr(2,4,2) = 0.0d0
  factors%get_supernode_ptr(3,1,1) = 0.0d0
  factors%get_supernode_ptr(3,2,1) = 0.0d0
  factors%get_supernode_ptr(3,2,2) = 0.0d0

  factors%get_border_ptr(1,1,1) = 0.0d0
  factors%get_border_ptr(1,2,1) = 0.0d0
  factors%get_border_ptr(2,1,1) = 0.0d0

  factors%get_work_ptr(1,1,1) = 0d0
  factors%get_work_ptr(1,2,1) = 0d0
  factors%get_work_ptr(1,2,2) = 0d0
  factors%get_work_ptr(2,3,3) = 0d0
  factors%get_work_ptr(2,4,3) = 0d0
  factors%get_work_ptr(2,4,4) = 0d0

  factors%get_matrix_ptr(1, 1, 1) = factors%get_matrix_ptr(1, 1, 1) + 1d0
  factors%get_matrix_ptr(1, 2, 1) = factors%get_matrix_ptr(1, 2, 1) + 1d0
  factors%get_matrix_ptr(1, 2, 2) = factors%get_matrix_ptr(1, 2, 2) + 1d0
  factors%get_matrix_ptr(2, 1, 1) = factors%get_matrix_ptr(2, 1, 1) + 1d0
  factors%get_matrix_ptr(2, 2, 1) = factors%get_matrix_ptr(2, 2, 1) + 1d0
  factors%get_matrix_ptr(2, 3, 1) = factors%get_matrix_ptr(2, 3, 1) + 1d0
  factors%get_matrix_ptr(2, 4, 1) = factors%get_matrix_ptr(2, 4, 1) + 1d0
  factors%get_matrix_ptr(2, 2, 2) = factors%get_matrix_ptr(2, 2, 2) + 1d0
  factors%get_matrix_ptr(2, 3, 2) = factors%get_matrix_ptr(2, 3, 2) + 1d0
  factors%get_matrix_ptr(2, 4, 2) = factors%get_matrix_ptr(2, 4, 2) + 1d0
  factors%get_matrix_ptr(2, 3, 3) = factors%get_matrix_ptr(2, 3, 3) + 1d0
  factors%get_matrix_ptr(2, 4, 3) = factors%get_matrix_ptr(2, 4, 3) + 1d0
  factors%get_matrix_ptr(2, 4, 4) = factors%get_matrix_ptr(2, 4, 4) + 1d0
  factors%get_matrix_ptr(3, 1, 1) = factors%get_matrix_ptr(3, 1, 1) + 1d0
  factors%get_matrix_ptr(3, 2, 1) = factors%get_matrix_ptr(3, 2, 1) + 1d0
  factors%get_matrix_ptr(3, 2, 2) = factors%get_matrix_ptr(3, 2, 2) + 1d0

  call start_array_tests("matrix_ptr:supernode")
  call add_test("(node, i, j) = (1,1,1)", factors%get_supernode_ptr(1,1,1), [(0d0, i=1,3)])
  call add_test("(node, i, j) = (1,2,1)", factors%get_supernode_ptr(1,2,1), [(0d0, i=1,2)])
  call add_test("(node, i, j) = (2,1,1)", factors%get_supernode_ptr(2,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,2,1)", factors%get_supernode_ptr(2,2,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,3,1)", factors%get_supernode_ptr(2,3,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,1)", factors%get_supernode_ptr(2,4,1), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (2,2,2)", factors%get_supernode_ptr(2,2,2), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,3,2)", factors%get_supernode_ptr(2,3,2), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,2)", factors%get_supernode_ptr(2,4,2), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (3,1,1)", factors%get_supernode_ptr(3,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (3,2,1)", factors%get_supernode_ptr(3,2,1), [(1d0, i=1,3)])
  call add_test("(node, i, j) = (3,2,2)", factors%get_supernode_ptr(3,2,2), [(1d0, i=1,1)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:border")
  call add_test("(node, i, j) = (1,1,1)", factors%get_border_ptr(1,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (1,2,1)", factors%get_border_ptr(1,2,1), [(1d0, i=1,6)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:work")
  call add_test("(node, i, j) = (1,1,1)", factors%get_work_ptr(1,1,1), [(0d0, i=1,4)])
  call add_test("(node, i, j) = (1,2,1)", factors%get_work_ptr(1,2,1), [(0d0, i=1,4)])
  call add_test("(node, i, j) = (1,2,2)", factors%get_work_ptr(1,2,2), [(1d0, i=1,4)])
  call add_test("(node, i, j) = (2,3,3)", factors%get_work_ptr(2,3,3), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,3)", factors%get_work_ptr(2,4,3), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (2,4,4)", factors%get_work_ptr(2,4,4), [(1d0, i=1,4)])
  call end_array_tests()
  
  nb = 3
  node_sets => create_contiguous_sets([5, 6, 7, 5, 3, 6])
  ccs => create_jagged_array([5, 4, 4, 4, 6, 0])
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  factors => create_factors(node_data, nb)

  factors%get_supernode_ptr(1,1,1) = 0d0
  factors%get_supernode_ptr(1,2,1) = 0d0
  factors%get_supernode_ptr(1,3,1) = 0d0
  factors%get_supernode_ptr(1,4,1) = 0d0
  factors%get_supernode_ptr(1,2,2) = 0d0
  factors%get_supernode_ptr(1,3,2) = 0d0
  factors%get_supernode_ptr(1,4,2) = 0d0
  factors%get_supernode_ptr(2,1,1) = 0d0
  factors%get_supernode_ptr(2,2,1) = 0d0
  factors%get_supernode_ptr(2,3,1) = 0d0
  factors%get_supernode_ptr(2,4,1) = 0d0
  factors%get_supernode_ptr(2,2,2) = 0d0
  factors%get_supernode_ptr(2,3,2) = 0d0
  factors%get_supernode_ptr(2,4,2) = 0d0
  factors%get_supernode_ptr(3,1,1) = 0d0
  factors%get_supernode_ptr(3,2,1) = 0d0
  factors%get_supernode_ptr(3,3,1) = 0d0
  factors%get_supernode_ptr(3,4,1) = 0d0
  factors%get_supernode_ptr(3,2,2) = 0d0
  factors%get_supernode_ptr(3,3,2) = 0d0
  factors%get_supernode_ptr(3,4,2) = 0d0
  factors%get_supernode_ptr(3,3,3) = 0d0
  factors%get_supernode_ptr(3,4,3) = 0d0
  factors%get_supernode_ptr(4,1,1) = 0d0
  factors%get_supernode_ptr(4,2,1) = 0d0
  factors%get_supernode_ptr(4,3,1) = 0d0
  factors%get_supernode_ptr(4,2,2) = 0d0
  factors%get_supernode_ptr(4,3,2) = 0d0
  factors%get_supernode_ptr(5,1,1) = 0d0
  factors%get_supernode_ptr(5,2,1) = 0d0
  factors%get_supernode_ptr(5,3,1) = 0d0
  factors%get_supernode_ptr(6,1,1) = 0d0
  factors%get_supernode_ptr(6,2,1) = 0d0
  factors%get_supernode_ptr(6,2,2) = 0d0

  factors%get_border_ptr(1,2,2) = 0d0
  factors%get_border_ptr(1,3,2) = 0d0
  factors%get_border_ptr(1,4,2) = 0d0
  factors%get_border_ptr(3,3,3) = 0d0
  factors%get_border_ptr(3,4,3) = 0d0
  factors%get_border_ptr(4,2,2) = 0d0
  factors%get_border_ptr(4,3,2) = 0d0

  factors%get_work_ptr(1,2,2) = 0d0
  factors%get_work_ptr(1,3,2) = 0d0
  factors%get_work_ptr(1,4,2) = 0d0
  factors%get_work_ptr(1,3,3) = 0d0
  factors%get_work_ptr(1,4,3) = 0d0
  factors%get_work_ptr(1,4,4) = 0d0
  factors%get_work_ptr(2,3,3) = 0d0
  factors%get_work_ptr(2,4,3) = 0d0
  factors%get_work_ptr(2,4,4) = 0d0
  factors%get_work_ptr(3,3,3) = 0d0
  factors%get_work_ptr(3,4,3) = 0d0
  factors%get_work_ptr(3,4,4) = 0d0
  factors%get_work_ptr(4,2,2) = 0d0
  factors%get_work_ptr(4,3,2) = 0d0
  factors%get_work_ptr(4,3,3) = 0d0
  factors%get_work_ptr(5,2,2) = 0d0
  factors%get_work_ptr(5,3,2) = 0d0
  factors%get_work_ptr(5,3,3) = 0d0

  factors%get_matrix_ptr(1,1,1) = factors%get_matrix_ptr(1,1,1) + 1d0
  factors%get_matrix_ptr(1,2,1) = factors%get_matrix_ptr(1,2,1) + 1d0
  factors%get_matrix_ptr(1,3,1) = factors%get_matrix_ptr(1,3,1) + 1d0
  factors%get_matrix_ptr(1,4,1) = factors%get_matrix_ptr(1,4,1) + 1d0
  factors%get_matrix_ptr(1,2,2) = factors%get_matrix_ptr(1,2,2) + 1d0
  factors%get_matrix_ptr(1,3,2) = factors%get_matrix_ptr(1,3,2) + 1d0
  factors%get_matrix_ptr(1,4,2) = factors%get_matrix_ptr(1,4,2) + 1d0
  factors%get_matrix_ptr(1,3,3) = factors%get_matrix_ptr(1,3,3) + 1d0
  factors%get_matrix_ptr(1,4,3) = factors%get_matrix_ptr(1,4,3) + 1d0
  factors%get_matrix_ptr(1,4,4) = factors%get_matrix_ptr(1,4,4) + 1d0
  factors%get_matrix_ptr(2,1,1) = factors%get_matrix_ptr(2,1,1) + 1d0
  factors%get_matrix_ptr(2,2,1) = factors%get_matrix_ptr(2,2,1) + 1d0
  factors%get_matrix_ptr(2,3,1) = factors%get_matrix_ptr(2,3,1) + 1d0
  factors%get_matrix_ptr(2,4,1) = factors%get_matrix_ptr(2,4,1) + 1d0
  factors%get_matrix_ptr(2,2,2) = factors%get_matrix_ptr(2,2,2) + 1d0
  factors%get_matrix_ptr(2,3,2) = factors%get_matrix_ptr(2,3,2) + 1d0
  factors%get_matrix_ptr(2,4,2) = factors%get_matrix_ptr(2,4,2) + 1d0
  factors%get_matrix_ptr(2,3,3) = factors%get_matrix_ptr(2,3,3) + 1d0
  factors%get_matrix_ptr(2,4,3) = factors%get_matrix_ptr(2,4,3) + 1d0
  factors%get_matrix_ptr(2,4,4) = factors%get_matrix_ptr(2,4,4) + 1d0
  factors%get_matrix_ptr(3,1,1) = factors%get_matrix_ptr(3,1,1) + 1d0
  factors%get_matrix_ptr(3,2,1) = factors%get_matrix_ptr(3,2,1) + 1d0
  factors%get_matrix_ptr(3,3,1) = factors%get_matrix_ptr(3,3,1) + 1d0
  factors%get_matrix_ptr(3,4,1) = factors%get_matrix_ptr(3,4,1) + 1d0
  factors%get_matrix_ptr(3,2,2) = factors%get_matrix_ptr(3,2,2) + 1d0
  factors%get_matrix_ptr(3,3,2) = factors%get_matrix_ptr(3,3,2) + 1d0
  factors%get_matrix_ptr(3,4,2) = factors%get_matrix_ptr(3,4,2) + 1d0
  factors%get_matrix_ptr(3,3,3) = factors%get_matrix_ptr(3,3,3) + 1d0
  factors%get_matrix_ptr(3,4,3) = factors%get_matrix_ptr(3,4,3) + 1d0
  factors%get_matrix_ptr(3,4,4) = factors%get_matrix_ptr(3,4,4) + 1d0
  factors%get_matrix_ptr(4,1,1) = factors%get_matrix_ptr(4,1,1) + 1d0
  factors%get_matrix_ptr(4,2,1) = factors%get_matrix_ptr(4,2,1) + 1d0
  factors%get_matrix_ptr(4,3,1) = factors%get_matrix_ptr(4,3,1) + 1d0
  factors%get_matrix_ptr(4,2,2) = factors%get_matrix_ptr(4,2,2) + 1d0
  factors%get_matrix_ptr(4,3,2) = factors%get_matrix_ptr(4,3,2) + 1d0
  factors%get_matrix_ptr(4,3,3) = factors%get_matrix_ptr(4,3,3) + 1d0
  factors%get_matrix_ptr(5,1,1) = factors%get_matrix_ptr(5,1,1) + 1d0
  factors%get_matrix_ptr(5,2,1) = factors%get_matrix_ptr(5,2,1) + 1d0
  factors%get_matrix_ptr(5,3,1) = factors%get_matrix_ptr(5,3,1) + 1d0
  factors%get_matrix_ptr(5,2,2) = factors%get_matrix_ptr(5,2,2) + 1d0
  factors%get_matrix_ptr(5,3,2) = factors%get_matrix_ptr(5,3,2) + 1d0
  factors%get_matrix_ptr(5,3,3) = factors%get_matrix_ptr(5,3,3) + 1d0
  factors%get_matrix_ptr(6,1,1) = factors%get_matrix_ptr(6,1,1) + 1d0
  factors%get_matrix_ptr(6,2,1) = factors%get_matrix_ptr(6,2,1) + 1d0
  factors%get_matrix_ptr(6,2,2) = factors%get_matrix_ptr(6,2,2) + 1d0
  

  call start_array_tests("matrix_ptr:supernode")
  call add_test("(1,1,1)", factors%get_supernode_ptr(1,1,1), [(1d0, i=1,9)])
  call add_test("(1,2,1)", factors%get_supernode_ptr(1,2,1), [(1d0, i=1,9)])
  call add_test("(1,3,1)", factors%get_supernode_ptr(1,3,1), [(1d0, i=1,9)])
  call add_test("(1,4,1)", factors%get_supernode_ptr(1,4,1), [(1d0, i=1,3)])
  call add_test("(1,2,2)", factors%get_supernode_ptr(1,2,2), [(0d0, i=1,6)])
  call add_test("(1,3,2)", factors%get_supernode_ptr(1,3,2), [(0d0, i=1,6)])
  call add_test("(1,4,2)", factors%get_supernode_ptr(1,4,2), [(0d0, i=1,2)])
  call add_test("(2,1,1)", factors%get_supernode_ptr(2,1,1), [(1d0, i=1,9)])
  call add_test("(2,2,1)", factors%get_supernode_ptr(2,2,1), [(1d0, i=1,9)])
  call add_test("(2,3,1)", factors%get_supernode_ptr(2,3,1), [(1d0, i=1,9)])
  call add_test("(2,4,1)", factors%get_supernode_ptr(2,4,1), [(1d0, i=1,3)])
  call add_test("(2,2,2)", factors%get_supernode_ptr(2,2,2), [(1d0, i=1,9)])
  call add_test("(2,3,2)", factors%get_supernode_ptr(2,3,2), [(1d0, i=1,9)])
  call add_test("(2,4,2)", factors%get_supernode_ptr(2,4,2), [(1d0, i=1,3)])
  call add_test("(3,1,1)", factors%get_supernode_ptr(3,1,1), [(1d0, i=1,9)])
  call add_test("(3,2,1)", factors%get_supernode_ptr(3,2,1), [(1d0, i=1,9)])
  call add_test("(3,3,1)", factors%get_supernode_ptr(3,3,1), [(1d0, i=1,9)])
  call add_test("(3,4,1)", factors%get_supernode_ptr(3,4,1), [(1d0, i=1,6)])
  call add_test("(3,2,2)", factors%get_supernode_ptr(3,2,2), [(1d0, i=1,9)])
  call add_test("(3,3,2)", factors%get_supernode_ptr(3,3,2), [(1d0, i=1,9)])
  call add_test("(3,4,2)", factors%get_supernode_ptr(3,4,2), [(1d0, i=1,6)])
  call add_test("(3,3,3)", factors%get_supernode_ptr(3,3,3), [(0d0, i=1,3)])
  call add_test("(3,4,3)", factors%get_supernode_ptr(3,4,3), [(0d0, i=1,2)])
  call add_test("(4,1,1)", factors%get_supernode_ptr(4,1,1), [(1d0, i=1,9)])
  call add_test("(4,2,1)", factors%get_supernode_ptr(4,2,1), [(1d0, i=1,9)])
  call add_test("(4,3,1)", factors%get_supernode_ptr(4,3,1), [(1d0, i=1,9)])
  call add_test("(4,2,2)", factors%get_supernode_ptr(4,2,2), [(0d0, i=1,6)])
  call add_test("(4,3,2)", factors%get_supernode_ptr(4,3,2), [(0d0, i=1,6)])
  call add_test("(5,1,1)", factors%get_supernode_ptr(5,1,1), [(1d0, i=1,9)])
  call add_test("(5,2,1)", factors%get_supernode_ptr(5,2,1), [(1d0, i=1,9)])
  call add_test("(5,3,1)", factors%get_supernode_ptr(5,3,1), [(1d0, i=1,9)])
  call add_test("(6,1,1)", factors%get_supernode_ptr(6,1,1), [(1d0, i=1,9)])
  call add_test("(6,2,1)", factors%get_supernode_ptr(6,2,1), [(1d0, i=1,9)])
  call add_test("(6,2,2)", factors%get_supernode_ptr(6,2,2), [(1d0, i=1,9)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:border")
  call add_test("(1,2,2)", factors%get_border_ptr(1,2,2), [(1d0, i=1,9)])
  call add_test("(1,3,2)", factors%get_border_ptr(1,3,2), [(1d0, i=1,9)])
  call add_test("(1,4,2)", factors%get_border_ptr(1,4,2), [(1d0, i=1,3)])
  call add_test("(3,3,3)", factors%get_border_ptr(3,3,3), [(1d0, i=1,9)])
  call add_test("(3,4,3)", factors%get_border_ptr(3,4,3), [(1d0, i=1,6)])
  call add_test("(4,2,2)", factors%get_border_ptr(4,2,2), [(1d0, i=1,9)])
  call add_test("(4,3,2)", factors%get_border_ptr(4,3,2), [(1d0, i=1,9)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:work")
  call add_test("(1,2,2)", factors%get_work_ptr(1,2,2), [(0d0, i=1,1)])
  call add_test("(1,3,2)", factors%get_work_ptr(1,3,2), [(0d0, i=1,3)])
  call add_test("(1,4,2)", factors%get_work_ptr(1,4,2), [(0d0, i=1,1)])
  call add_test("(1,3,3)", factors%get_work_ptr(1,3,3), [(1d0, i=1,9)])
  call add_test("(1,4,3)", factors%get_work_ptr(1,4,3), [(1d0, i=1,3)])
  call add_test("(1,4,4)", factors%get_work_ptr(1,4,4), [(1d0, i=1,1)])
  call add_test("(2,3,3)", factors%get_work_ptr(2,3,3), [(1d0, i=1,9)])
  call add_test("(2,4,3)", factors%get_work_ptr(2,4,3), [(1d0, i=1,3)])
  call add_test("(2,4,4)", factors%get_work_ptr(2,4,4), [(1d0, i=1,1)])
  call add_test("(3,3,3)", factors%get_work_ptr(3,3,3), [(0d0, i=1,4)])
  call add_test("(3,4,3)", factors%get_work_ptr(3,4,3), [(0d0, i=1,4)])
  call add_test("(3,4,4)", factors%get_work_ptr(3,4,4), [(1d0, i=1,4)])
  call add_test("(4,2,2)", factors%get_work_ptr(4,2,2), [(0d0, i=1,1)])
  call add_test("(4,3,2)", factors%get_work_ptr(4,3,2), [(0d0, i=1,3)])
  call add_test("(4,3,3)", factors%get_work_ptr(4,3,3), [(1d0, i=1,9)])
  call add_test("(5,2,2)", factors%get_work_ptr(5,2,2), [(1d0, i=1,9)])
  call add_test("(5,3,2)", factors%get_work_ptr(5,3,2), [(1d0, i=1,9)])
  call add_test("(5,3,3)", factors%get_work_ptr(5,3,3), [(1d0, i=1,9)])
  call end_array_tests()

end program factors_test

