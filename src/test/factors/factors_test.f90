program factors_test
  use factors_m
  use test_util
  use node_data_m
  implicit none
  type(factors_c), pointer :: factors
  type(node_data_c), pointer :: node_data
  integer :: nb, i

  nb = 3
  node_data => create_node_data([1,6,4], [4,5,0], nb)
  factors => create_factors(node_data)

  factors%get_supernode(1,1,1) = 0.0d0
  factors%get_supernode(1,2,1) = 0.0d0
  factors%get_supernode(2,1,1) = 0.0d0
  factors%get_supernode(2,2,1) = 0.0d0
  factors%get_supernode(2,3,1) = 0.0d0
  factors%get_supernode(2,4,1) = 0.0d0
  factors%get_supernode(2,2,2) = 0.0d0
  factors%get_supernode(2,3,2) = 0.0d0
  factors%get_supernode(2,4,2) = 0.0d0
  factors%get_supernode(3,1,1) = 0.0d0
  factors%get_supernode(3,2,1) = 0.0d0
  factors%get_supernode(3,2,2) = 0.0d0

  factors%get_border(1,1,1) = 0.0d0
  factors%get_border(1,2,1) = 0.0d0
  factors%get_border(2,1,1) = 0.0d0

  factors%get_work(1,1,1) = 0d0
  factors%get_work(1,2,1) = 0d0
  factors%get_work(1,2,2) = 0d0
  factors%get_work(2,3,3) = 0d0
  factors%get_work(2,4,3) = 0d0
  factors%get_work(2,4,4) = 0d0

  factors%get_matrix(1, 1, 1) = factors%get_matrix(1, 1, 1) + 1d0
  factors%get_matrix(1, 2, 1) = factors%get_matrix(1, 2, 1) + 1d0
  factors%get_matrix(1, 2, 2) = factors%get_matrix(1, 2, 2) + 1d0
  factors%get_matrix(2, 1, 1) = factors%get_matrix(2, 1, 1) + 1d0
  factors%get_matrix(2, 2, 1) = factors%get_matrix(2, 2, 1) + 1d0
  factors%get_matrix(2, 3, 1) = factors%get_matrix(2, 3, 1) + 1d0
  factors%get_matrix(2, 4, 1) = factors%get_matrix(2, 4, 1) + 1d0
  factors%get_matrix(2, 2, 2) = factors%get_matrix(2, 2, 2) + 1d0
  factors%get_matrix(2, 3, 2) = factors%get_matrix(2, 3, 2) + 1d0
  factors%get_matrix(2, 4, 2) = factors%get_matrix(2, 4, 2) + 1d0
  factors%get_matrix(2, 3, 3) = factors%get_matrix(2, 3, 3) + 1d0
  factors%get_matrix(2, 4, 3) = factors%get_matrix(2, 4, 3) + 1d0
  factors%get_matrix(2, 4, 4) = factors%get_matrix(2, 4, 4) + 1d0
  factors%get_matrix(3, 1, 1) = factors%get_matrix(3, 1, 1) + 1d0
  factors%get_matrix(3, 2, 1) = factors%get_matrix(3, 2, 1) + 1d0
  factors%get_matrix(3, 2, 2) = factors%get_matrix(3, 2, 2) + 1d0

  call start_array_tests("matrix_ptr:supernode")
  call add_test("(node, i, j) = (1,1,1)", factors%get_supernode(1,1,1), [(0d0, i=1,3)])
  call add_test("(node, i, j) = (1,2,1)", factors%get_supernode(1,2,1), [(0d0, i=1,2)])
  call add_test("(node, i, j) = (2,1,1)", factors%get_supernode(2,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,2,1)", factors%get_supernode(2,2,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,3,1)", factors%get_supernode(2,3,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,1)", factors%get_supernode(2,4,1), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (2,2,2)", factors%get_supernode(2,2,2), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,3,2)", factors%get_supernode(2,3,2), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,2)", factors%get_supernode(2,4,2), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (3,1,1)", factors%get_supernode(3,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (3,2,1)", factors%get_supernode(3,2,1), [(1d0, i=1,3)])
  call add_test("(node, i, j) = (3,2,2)", factors%get_supernode(3,2,2), [(1d0, i=1,1)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:border")
  call add_test("(node, i, j) = (1,1,1)", factors%get_border(1,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (1,2,1)", factors%get_border(1,2,1), [(1d0, i=1,6)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:work")
  call add_test("(node, i, j) = (1,1,1)", factors%get_work(1,1,1), [(0d0, i=1,4)])
  call add_test("(node, i, j) = (1,2,1)", factors%get_work(1,2,1), [(0d0, i=1,4)])
  call add_test("(node, i, j) = (1,2,2)", factors%get_work(1,2,2), [(1d0, i=1,4)])
  call add_test("(node, i, j) = (2,3,3)", factors%get_work(2,3,3), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,4,3)", factors%get_work(2,4,3), [(1d0, i=1,6)])
  call add_test("(node, i, j) = (2,4,4)", factors%get_work(2,4,4), [(1d0, i=1,4)])
  call end_array_tests()
  
  nb = 3
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  factors => create_factors(node_data)

  factors%get_supernode(1,1,1) = 0d0
  factors%get_supernode(1,2,1) = 0d0
  factors%get_supernode(1,3,1) = 0d0
  factors%get_supernode(1,4,1) = 0d0
  factors%get_supernode(1,2,2) = 0d0
  factors%get_supernode(1,3,2) = 0d0
  factors%get_supernode(1,4,2) = 0d0
  factors%get_supernode(2,1,1) = 0d0
  factors%get_supernode(2,2,1) = 0d0
  factors%get_supernode(2,3,1) = 0d0
  factors%get_supernode(2,4,1) = 0d0
  factors%get_supernode(2,2,2) = 0d0
  factors%get_supernode(2,3,2) = 0d0
  factors%get_supernode(2,4,2) = 0d0
  factors%get_supernode(3,1,1) = 0d0
  factors%get_supernode(3,2,1) = 0d0
  factors%get_supernode(3,3,1) = 0d0
  factors%get_supernode(3,4,1) = 0d0
  factors%get_supernode(3,2,2) = 0d0
  factors%get_supernode(3,3,2) = 0d0
  factors%get_supernode(3,4,2) = 0d0
  factors%get_supernode(3,3,3) = 0d0
  factors%get_supernode(3,4,3) = 0d0
  factors%get_supernode(4,1,1) = 0d0
  factors%get_supernode(4,2,1) = 0d0
  factors%get_supernode(4,3,1) = 0d0
  factors%get_supernode(4,2,2) = 0d0
  factors%get_supernode(4,3,2) = 0d0
  factors%get_supernode(5,1,1) = 0d0
  factors%get_supernode(5,2,1) = 0d0
  factors%get_supernode(5,3,1) = 0d0
  factors%get_supernode(6,1,1) = 0d0
  factors%get_supernode(6,2,1) = 0d0
  factors%get_supernode(6,2,2) = 0d0

  factors%get_border(1,2,2) = 0d0
  factors%get_border(1,3,2) = 0d0
  factors%get_border(1,4,2) = 0d0
  factors%get_border(3,3,3) = 0d0
  factors%get_border(3,4,3) = 0d0
  factors%get_border(4,2,2) = 0d0
  factors%get_border(4,3,2) = 0d0

  factors%get_work(1,2,2) = 0d0
  factors%get_work(1,3,2) = 0d0
  factors%get_work(1,4,2) = 0d0
  factors%get_work(1,3,3) = 0d0
  factors%get_work(1,4,3) = 0d0
  factors%get_work(1,4,4) = 0d0
  factors%get_work(2,3,3) = 0d0
  factors%get_work(2,4,3) = 0d0
  factors%get_work(2,4,4) = 0d0
  factors%get_work(3,3,3) = 0d0
  factors%get_work(3,4,3) = 0d0
  factors%get_work(3,4,4) = 0d0
  factors%get_work(4,2,2) = 0d0
  factors%get_work(4,3,2) = 0d0
  factors%get_work(4,3,3) = 0d0
  factors%get_work(5,2,2) = 0d0
  factors%get_work(5,3,2) = 0d0
  factors%get_work(5,3,3) = 0d0

  factors%get_matrix(1,1,1) = factors%get_matrix(1,1,1) + 1d0
  factors%get_matrix(1,2,1) = factors%get_matrix(1,2,1) + 1d0
  factors%get_matrix(1,3,1) = factors%get_matrix(1,3,1) + 1d0
  factors%get_matrix(1,4,1) = factors%get_matrix(1,4,1) + 1d0
  factors%get_matrix(1,2,2) = factors%get_matrix(1,2,2) + 1d0
  factors%get_matrix(1,3,2) = factors%get_matrix(1,3,2) + 1d0
  factors%get_matrix(1,4,2) = factors%get_matrix(1,4,2) + 1d0
  factors%get_matrix(1,3,3) = factors%get_matrix(1,3,3) + 1d0
  factors%get_matrix(1,4,3) = factors%get_matrix(1,4,3) + 1d0
  factors%get_matrix(1,4,4) = factors%get_matrix(1,4,4) + 1d0
  factors%get_matrix(2,1,1) = factors%get_matrix(2,1,1) + 1d0
  factors%get_matrix(2,2,1) = factors%get_matrix(2,2,1) + 1d0
  factors%get_matrix(2,3,1) = factors%get_matrix(2,3,1) + 1d0
  factors%get_matrix(2,4,1) = factors%get_matrix(2,4,1) + 1d0
  factors%get_matrix(2,2,2) = factors%get_matrix(2,2,2) + 1d0
  factors%get_matrix(2,3,2) = factors%get_matrix(2,3,2) + 1d0
  factors%get_matrix(2,4,2) = factors%get_matrix(2,4,2) + 1d0
  factors%get_matrix(2,3,3) = factors%get_matrix(2,3,3) + 1d0
  factors%get_matrix(2,4,3) = factors%get_matrix(2,4,3) + 1d0
  factors%get_matrix(2,4,4) = factors%get_matrix(2,4,4) + 1d0
  factors%get_matrix(3,1,1) = factors%get_matrix(3,1,1) + 1d0
  factors%get_matrix(3,2,1) = factors%get_matrix(3,2,1) + 1d0
  factors%get_matrix(3,3,1) = factors%get_matrix(3,3,1) + 1d0
  factors%get_matrix(3,4,1) = factors%get_matrix(3,4,1) + 1d0
  factors%get_matrix(3,2,2) = factors%get_matrix(3,2,2) + 1d0
  factors%get_matrix(3,3,2) = factors%get_matrix(3,3,2) + 1d0
  factors%get_matrix(3,4,2) = factors%get_matrix(3,4,2) + 1d0
  factors%get_matrix(3,3,3) = factors%get_matrix(3,3,3) + 1d0
  factors%get_matrix(3,4,3) = factors%get_matrix(3,4,3) + 1d0
  factors%get_matrix(3,4,4) = factors%get_matrix(3,4,4) + 1d0
  factors%get_matrix(4,1,1) = factors%get_matrix(4,1,1) + 1d0
  factors%get_matrix(4,2,1) = factors%get_matrix(4,2,1) + 1d0
  factors%get_matrix(4,3,1) = factors%get_matrix(4,3,1) + 1d0
  factors%get_matrix(4,2,2) = factors%get_matrix(4,2,2) + 1d0
  factors%get_matrix(4,3,2) = factors%get_matrix(4,3,2) + 1d0
  factors%get_matrix(4,3,3) = factors%get_matrix(4,3,3) + 1d0
  factors%get_matrix(5,1,1) = factors%get_matrix(5,1,1) + 1d0
  factors%get_matrix(5,2,1) = factors%get_matrix(5,2,1) + 1d0
  factors%get_matrix(5,3,1) = factors%get_matrix(5,3,1) + 1d0
  factors%get_matrix(5,2,2) = factors%get_matrix(5,2,2) + 1d0
  factors%get_matrix(5,3,2) = factors%get_matrix(5,3,2) + 1d0
  factors%get_matrix(5,3,3) = factors%get_matrix(5,3,3) + 1d0
  factors%get_matrix(6,1,1) = factors%get_matrix(6,1,1) + 1d0
  factors%get_matrix(6,2,1) = factors%get_matrix(6,2,1) + 1d0
  factors%get_matrix(6,2,2) = factors%get_matrix(6,2,2) + 1d0
  

  call start_array_tests("matrix_ptr:supernode")
  call add_test("(1,1,1)", factors%get_supernode(1,1,1), [(1d0, i=1,9)])
  call add_test("(1,2,1)", factors%get_supernode(1,2,1), [(1d0, i=1,9)])
  call add_test("(1,3,1)", factors%get_supernode(1,3,1), [(1d0, i=1,9)])
  call add_test("(1,4,1)", factors%get_supernode(1,4,1), [(1d0, i=1,3)])
  call add_test("(1,2,2)", factors%get_supernode(1,2,2), [(0d0, i=1,6)])
  call add_test("(1,3,2)", factors%get_supernode(1,3,2), [(0d0, i=1,6)])
  call add_test("(1,4,2)", factors%get_supernode(1,4,2), [(0d0, i=1,2)])
  call add_test("(2,1,1)", factors%get_supernode(2,1,1), [(1d0, i=1,9)])
  call add_test("(2,2,1)", factors%get_supernode(2,2,1), [(1d0, i=1,9)])
  call add_test("(2,3,1)", factors%get_supernode(2,3,1), [(1d0, i=1,9)])
  call add_test("(2,4,1)", factors%get_supernode(2,4,1), [(1d0, i=1,3)])
  call add_test("(2,2,2)", factors%get_supernode(2,2,2), [(1d0, i=1,9)])
  call add_test("(2,3,2)", factors%get_supernode(2,3,2), [(1d0, i=1,9)])
  call add_test("(2,4,2)", factors%get_supernode(2,4,2), [(1d0, i=1,3)])
  call add_test("(3,1,1)", factors%get_supernode(3,1,1), [(1d0, i=1,9)])
  call add_test("(3,2,1)", factors%get_supernode(3,2,1), [(1d0, i=1,9)])
  call add_test("(3,3,1)", factors%get_supernode(3,3,1), [(1d0, i=1,9)])
  call add_test("(3,4,1)", factors%get_supernode(3,4,1), [(1d0, i=1,6)])
  call add_test("(3,2,2)", factors%get_supernode(3,2,2), [(1d0, i=1,9)])
  call add_test("(3,3,2)", factors%get_supernode(3,3,2), [(1d0, i=1,9)])
  call add_test("(3,4,2)", factors%get_supernode(3,4,2), [(1d0, i=1,6)])
  call add_test("(3,3,3)", factors%get_supernode(3,3,3), [(0d0, i=1,3)])
  call add_test("(3,4,3)", factors%get_supernode(3,4,3), [(0d0, i=1,2)])
  call add_test("(4,1,1)", factors%get_supernode(4,1,1), [(1d0, i=1,9)])
  call add_test("(4,2,1)", factors%get_supernode(4,2,1), [(1d0, i=1,9)])
  call add_test("(4,3,1)", factors%get_supernode(4,3,1), [(1d0, i=1,9)])
  call add_test("(4,2,2)", factors%get_supernode(4,2,2), [(0d0, i=1,6)])
  call add_test("(4,3,2)", factors%get_supernode(4,3,2), [(0d0, i=1,6)])
  call add_test("(5,1,1)", factors%get_supernode(5,1,1), [(1d0, i=1,9)])
  call add_test("(5,2,1)", factors%get_supernode(5,2,1), [(1d0, i=1,9)])
  call add_test("(5,3,1)", factors%get_supernode(5,3,1), [(1d0, i=1,9)])
  call add_test("(6,1,1)", factors%get_supernode(6,1,1), [(1d0, i=1,9)])
  call add_test("(6,2,1)", factors%get_supernode(6,2,1), [(1d0, i=1,9)])
  call add_test("(6,2,2)", factors%get_supernode(6,2,2), [(1d0, i=1,9)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:border")
  call add_test("(1,2,2)", factors%get_border(1,2,2), [(1d0, i=1,9)])
  call add_test("(1,3,2)", factors%get_border(1,3,2), [(1d0, i=1,9)])
  call add_test("(1,4,2)", factors%get_border(1,4,2), [(1d0, i=1,3)])
  call add_test("(3,3,3)", factors%get_border(3,3,3), [(1d0, i=1,9)])
  call add_test("(3,4,3)", factors%get_border(3,4,3), [(1d0, i=1,6)])
  call add_test("(4,2,2)", factors%get_border(4,2,2), [(1d0, i=1,9)])
  call add_test("(4,3,2)", factors%get_border(4,3,2), [(1d0, i=1,9)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:work")
  call add_test("(1,2,2)", factors%get_work(1,2,2), [(0d0, i=1,1)])
  call add_test("(1,3,2)", factors%get_work(1,3,2), [(0d0, i=1,3)])
  call add_test("(1,4,2)", factors%get_work(1,4,2), [(0d0, i=1,1)])
  call add_test("(1,3,3)", factors%get_work(1,3,3), [(1d0, i=1,9)])
  call add_test("(1,4,3)", factors%get_work(1,4,3), [(1d0, i=1,3)])
  call add_test("(1,4,4)", factors%get_work(1,4,4), [(1d0, i=1,1)])
  call add_test("(2,3,3)", factors%get_work(2,3,3), [(1d0, i=1,9)])
  call add_test("(2,4,3)", factors%get_work(2,4,3), [(1d0, i=1,3)])
  call add_test("(2,4,4)", factors%get_work(2,4,4), [(1d0, i=1,1)])
  call add_test("(3,3,3)", factors%get_work(3,3,3), [(0d0, i=1,4)])
  call add_test("(3,4,3)", factors%get_work(3,4,3), [(0d0, i=1,4)])
  call add_test("(3,4,4)", factors%get_work(3,4,4), [(1d0, i=1,4)])
  call add_test("(4,2,2)", factors%get_work(4,2,2), [(0d0, i=1,1)])
  call add_test("(4,3,2)", factors%get_work(4,3,2), [(0d0, i=1,3)])
  call add_test("(4,3,3)", factors%get_work(4,3,3), [(1d0, i=1,9)])
  call add_test("(5,2,2)", factors%get_work(5,2,2), [(1d0, i=1,9)])
  call add_test("(5,3,2)", factors%get_work(5,3,2), [(1d0, i=1,9)])
  call add_test("(5,3,3)", factors%get_work(5,3,3), [(1d0, i=1,9)])
  call end_array_tests()

  node_data => create_node_data([6], [0], 3)
  factors => create_factors(node_data)

  factors%get_matrix(1,1,1) = 0d0
  factors%get_matrix(1,2,1) = 0d0
  factors%get_matrix(1,2,2) = 0d0

  factors%get_matrix(1,1,1) = factors%get_matrix(1,1,1) + 1d0
  factors%get_matrix(1,2,1) = factors%get_matrix(1,2,1) + 1d0
  factors%get_matrix(1,2,2) = factors%get_matrix(1,2,2) + 1d0
  
  call start_array_tests("最後のノードがdivisibleの場合:nb=3, supernode_size=6")
  call add_test("(node, i, j) = (1,1,1)", factors%get_supernode(1,1,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (1,2,1)", factors%get_supernode(1,2,1), [(1d0, i=1,9)])
  call add_test("(node, i, j) = (2,2,2)", factors%get_supernode(1,2,2), [(1d0, i=1,9)])
  call end_array_tests()


end program factors_test

