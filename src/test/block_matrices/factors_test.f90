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
  factors => create_factors(node_data, ccs, nb)

  call assert_equal("num_node", factors%get_num_node(), 3)
  
  call start_tests("num_block")
  call add_test("node=1", factors%get_num_block(1), 2)
  call add_test("node=2", factors%get_num_block(2), 4)
  call add_test("node=3", factors%get_num_block(3), 2)
  call end_tests()

  call start_tests("work_start_index")
  call add_test("node=1", factors%get_work_start_index(1), 1)
  call add_test("node=2", factors%get_work_start_index(2), 3)
  call end_tests()

  call start_tests("exist_border")
  call add_test("node=1", factors%exist_border(1), .true.)
  call add_test("node=2", factors%exist_border(2), .false.)
  call add_test("node=3", factors%exist_border(3), .true.)
  call end_tests()

  call start_tests("get_block_size")
  call add_test("node=1, index=1", factors%get_block_size(1, 1), 3)
  call add_test("node=1, index=2", factors%get_block_size(2, 1), 2)
  call add_test("node=2, index=1", factors%get_block_size(1, 2), 3)
  call add_test("node=2, index=2", factors%get_block_size(2, 2), 3)
  call add_test("node=2, index=3", factors%get_block_size(3, 2), 3)
  call add_test("node=2, index=4", factors%get_block_size(4, 2), 2)
  call add_test("node=3, index=1", factors%get_block_size(1, 3), 3)
  call add_test("node=3, index=2", factors%get_block_size(2, 3), 1)
  call end_tests()

  call start_tests("get_supernode_block_size")
  call add_test("node=1, index=1", factors%get_supernode_block_size(1, 1), 1)
  call add_test("node=2, index=1", factors%get_supernode_block_size(1, 2), 3)
  call add_test("node=2, index=2", factors%get_supernode_block_size(2, 2), 3)
  call add_test("node=3, index=1", factors%get_supernode_block_size(1, 3), 3)
  call add_test("node=3, index=2", factors%get_supernode_block_size(2, 3), 1)
  call end_tests()

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
  
  call start_tests("border_info")
  call factors%get_border_info(1, ssize, wsize)
  call add_test("node=1:ssize", ssize, 1)
  call add_test("node=1:wsize", wsize, 2)
  call end_tests()

  call start_tests("get_first")
  call add_test("node=1", factors%get_first(1), 1)
  call add_test("node=2", factors%get_first(2), 2)
  call add_test("node=3", factors%get_first(3), 8)
  call end_tests()

  call start_tests("get_last")
  call add_test("node=1", factors%get_last(1), 1)
  call add_test("node=2", factors%get_last(2), 7)
  call add_test("node=3", factors%get_last(3), 11)
  call end_tests()
  
  nb = 3
  node_sets => create_contiguous_sets([5, 6, 7, 5, 3, 6])
  ccs => create_jagged_array([5, 4, 4, 4, 6, 0])
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  factors => create_factors(node_data, ccs, nb)

  call assert_equal("num_node", factors%get_num_node(), 6)
  
  call start_tests("num_block")
  call add_test("node=1", factors%get_num_block(1), 4)
  call add_test("node=2", factors%get_num_block(2), 4)
  call add_test("node=3", factors%get_num_block(3), 4)
  call add_test("node=4", factors%get_num_block(4), 3)
  call add_test("node=5", factors%get_num_block(5), 3)
  call add_test("node=6", factors%get_num_block(6), 2)
  call end_tests()

  call start_tests("work_start_index")
  call add_test("node=1", factors%get_work_start_index(1), 2)
  call add_test("node=2", factors%get_work_start_index(2), 3)
  call add_test("node=3", factors%get_work_start_index(3), 3)
  call add_test("node=4", factors%get_work_start_index(4), 2)
  call add_test("node=5", factors%get_work_start_index(5), 2)
  call end_tests()

  call start_tests("exist_border")
  call add_test("node=1", factors%exist_border(1), .true.)
  call add_test("node=2", factors%exist_border(2), .false.)
  call add_test("node=3", factors%exist_border(3), .true.)
  call add_test("node=4", factors%exist_border(4), .true.)
  call add_test("node=5", factors%exist_border(5), .false.)
  call add_test("node=6", factors%exist_border(6), .false.)
  call end_tests()
  
  call start_tests("get_block_size")
  call add_test("node=1, index=1", factors%get_block_size(1, 1), 3)
  call add_test("node=1, index=2", factors%get_block_size(2, 1), 3)
  call add_test("node=1, index=3", factors%get_block_size(3, 1), 3)
  call add_test("node=1, index=4", factors%get_block_size(4, 1), 1)
  call add_test("node=2, index=1", factors%get_block_size(1, 2), 3)
  call add_test("node=2, index=2", factors%get_block_size(2, 2), 3)
  call add_test("node=2, index=3", factors%get_block_size(3, 2), 3)
  call add_test("node=2, index=4", factors%get_block_size(4, 2), 1)
  call add_test("node=3, index=1", factors%get_block_size(1, 3), 3)
  call add_test("node=3, index=2", factors%get_block_size(2, 3), 3)
  call add_test("node=3, index=3", factors%get_block_size(3, 3), 3)
  call add_test("node=3, index=4", factors%get_block_size(4, 3), 2)
  call add_test("node=4, index=1", factors%get_block_size(1, 4), 3)
  call add_test("node=4, index=2", factors%get_block_size(2, 4), 3)
  call add_test("node=4, index=3", factors%get_block_size(3, 4), 3)
  call add_test("node=5, index=1", factors%get_block_size(1, 5), 3)
  call add_test("node=5, index=2", factors%get_block_size(2, 5), 3)
  call add_test("node=5, index=3", factors%get_block_size(3, 5), 3)
  call add_test("node=6, index=1", factors%get_block_size(1, 6), 3)
  call add_test("node=6, index=2", factors%get_block_size(2, 6), 3)
  call end_tests()

  call start_tests("get_suprnode_block_size")
  call add_test("node=1, index=1", factors%get_supernode_block_size(1, 1), 3)
  call add_test("node=1, index=2", factors%get_supernode_block_size(2, 1), 2)
  call add_test("node=2, index=1", factors%get_supernode_block_size(1, 2), 3)
  call add_test("node=2, index=2", factors%get_supernode_block_size(2, 2), 3)
  call add_test("node=3, index=1", factors%get_supernode_block_size(1, 3), 3)
  call add_test("node=3, index=2", factors%get_supernode_block_size(2, 3), 3)
  call add_test("node=3, index=3", factors%get_supernode_block_size(3, 3), 1)
  call add_test("node=4, index=1", factors%get_supernode_block_size(1, 4), 3)
  call add_test("node=4, index=2", factors%get_supernode_block_size(2, 4), 2)
  call add_test("node=5, index=1", factors%get_supernode_block_size(1, 5), 3)
  call add_test("node=6, index=1", factors%get_supernode_block_size(1, 6), 3)
  call add_test("node=6, index=2", factors%get_supernode_block_size(2, 6), 3)
  call end_tests()

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

  call start_array_tests("border_info")
  call factors%get_border_info(1, ssize, wsize)
  call add_test("node=1:ssize", ssize, 2)
  call add_test("node=1:wsize", wsize, 1)
  call factors%get_border_info(3, ssize, wsize)
  call add_test("node=3:ssize", ssize, 1)
  call add_test("node=3:wsize", wsize, 2)
  call factors%get_border_info(4, ssize, wsize)
  call add_test("node=4:ssize", ssize, 2)
  call add_test("node=4:wsize", wsize, 1)
  call end_array_tests()

  call start_tests("get_first")
  call add_test("node=1", factors%get_first(1), 1)
  call add_test("node=2", factors%get_first(2), 6)
  call add_test("node=3", factors%get_first(3), 12)
  call add_test("node=4", factors%get_first(4), 19)
  call add_test("node=5", factors%get_first(5), 24)
  call add_test("node=6", factors%get_first(6), 27)
  call end_tests()

  call start_tests("get_last")
  call add_test("node=1", factors%get_last(1), 5)
  call add_test("node=2", factors%get_last(2), 11)
  call add_test("node=3", factors%get_last(3), 18)
  call add_test("node=4", factors%get_last(4), 23)
  call add_test("node=5", factors%get_last(5), 26)
  call add_test("node=6", factors%get_last(6), 32)
  call end_tests()

end program factors_test

