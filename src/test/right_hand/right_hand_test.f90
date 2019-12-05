program right_hand_test
  use contiguous_sets_m
  use jagged_array_m
  use right_hand_m
  use test_util
  use node_data_m
  implicit none
  type(right_hand_c), pointer :: right_hand
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: ccs
  type(node_data_c), pointer :: node_data
  integer :: nb, i

  print *, "test1"
  nb = 3
  node_sets => create_contiguous_sets([1, 6, 4])
  ccs => create_jagged_array([4, 5, 0])
  node_data => create_node_data([1,6,4], [4,5,0], nb)
  right_hand => create_right_hand(node_data, node_sets, ccs, nb)

  right_hand%get_supernode_ptr(1,1) = 0.0d0
  right_hand%get_supernode_ptr(2,1) = 0.0d0
  right_hand%get_supernode_ptr(2,2) = 0.0d0
  right_hand%get_supernode_ptr(3,1) = 0.0d0
  right_hand%get_supernode_ptr(3,2) = 0.0d0

  right_hand%get_border_ptr(1,1) = 0.0d0

  right_hand%get_work_ptr(1,1) = 0.0d0
  right_hand%get_work_ptr(1,2) = 0.0d0
  right_hand%get_work_ptr(2,3) = 0.0d0
  right_hand%get_work_ptr(2,4) = 0.0d0

  right_hand%get_array_ptr(1,1) = right_hand%get_array_ptr(1,1) + 1d0
  right_hand%get_array_ptr(1,2) = right_hand%get_array_ptr(1,2) + 1d0
  right_hand%get_array_ptr(2,1) = right_hand%get_array_ptr(2,1) + 1d0
  right_hand%get_array_ptr(2,2) = right_hand%get_array_ptr(2,2) + 1d0
  right_hand%get_array_ptr(2,3) = right_hand%get_array_ptr(2,3) + 1d0
  right_hand%get_array_ptr(2,4) = right_hand%get_array_ptr(2,4) + 1d0
  right_hand%get_array_ptr(3,1) = right_hand%get_array_ptr(3,1) + 1d0
  right_hand%get_array_ptr(3,2) = right_hand%get_array_ptr(3,2) + 1d0

  call start_array_tests("matrix_ptr:supernode")
  call add_test("node=1,index=1", right_hand%get_supernode_ptr(1,1), [(0d0, i=1,1)])
  call add_test("node=2,index=1", right_hand%get_supernode_ptr(2,1), [(1d0, i=1,3)])
  call add_test("node=2,index=2", right_hand%get_supernode_ptr(2,2), [(1d0, i=1,3)])
  call add_test("node=3,index=1", right_hand%get_supernode_ptr(3,1), [(1d0, i=1,3)])
  call add_test("node=3,index=2", right_hand%get_supernode_ptr(3,2), [(1d0, i=1,1)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:border")
  call add_test("node=1,index=1", right_hand%get_border_ptr(1,1), [(1d0, i=1,3)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:work")
  call add_test("node=1,index=1", right_hand%get_work_ptr(1,1), [(0d0, i=1,2)])
  call add_test("node=1,index=2", right_hand%get_work_ptr(1,2), [(1d0, i=1,2)])
  call add_test("node=2,index=3", right_hand%get_work_ptr(2,3), [(1d0, i=1,3)])
  call add_test("node=2,index=4", right_hand%get_work_ptr(2,4), [(1d0, i=1,2)])
  call end_array_tests()


  print *, "test2"
  nb = 3
  node_sets => create_contiguous_sets([5, 6, 7, 5, 3, 6])
  ccs => create_jagged_array([5, 4, 4, 4, 6, 0])
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  right_hand => create_right_hand(node_data, node_sets, ccs, nb)

  right_hand%get_supernode_ptr(1,1) = 0d0
  right_hand%get_supernode_ptr(1,2) = 0d0
  right_hand%get_supernode_ptr(2,1) = 0d0
  right_hand%get_supernode_ptr(2,2) = 0d0
  right_hand%get_supernode_ptr(3,1) = 0d0
  right_hand%get_supernode_ptr(3,2) = 0d0
  right_hand%get_supernode_ptr(3,3) = 0d0
  right_hand%get_supernode_ptr(4,1) = 0d0
  right_hand%get_supernode_ptr(4,2) = 0d0
  right_hand%get_supernode_ptr(5,1) = 0d0
  right_hand%get_supernode_ptr(6,1) = 0d0
  right_hand%get_supernode_ptr(6,2) = 0d0

  right_hand%get_border_ptr(1,2) = 0d0
  right_hand%get_border_ptr(3,3) = 0d0
  right_hand%get_border_ptr(4,2) = 0d0

  right_hand%get_work_ptr(1,2) = 0d0
  right_hand%get_work_ptr(1,3) = 0d0
  right_hand%get_work_ptr(1,4) = 0d0
  right_hand%get_work_ptr(2,3) = 0d0
  right_hand%get_work_ptr(2,4) = 0d0
  right_hand%get_work_ptr(3,3) = 0d0
  right_hand%get_work_ptr(3,4) = 0d0
  right_hand%get_work_ptr(4,2) = 0d0
  right_hand%get_work_ptr(4,3) = 0d0
  right_hand%get_work_ptr(5,2) = 0d0
  right_hand%get_work_ptr(5,3) = 0d0

  right_hand%get_array_ptr(1,1) = right_hand%get_array_ptr(1,1) + 1d0 
  right_hand%get_array_ptr(1,2) = right_hand%get_array_ptr(1,2) + 1d0
  right_hand%get_array_ptr(1,3) = right_hand%get_array_ptr(1,3) + 1d0
  right_hand%get_array_ptr(1,4) = right_hand%get_array_ptr(1,4) + 1d0
  right_hand%get_array_ptr(2,1) = right_hand%get_array_ptr(2,1) + 1d0
  right_hand%get_array_ptr(2,2) = right_hand%get_array_ptr(2,2) + 1d0
  right_hand%get_array_ptr(2,3) = right_hand%get_array_ptr(2,3) + 1d0
  right_hand%get_array_ptr(2,4) = right_hand%get_array_ptr(2,4) + 1d0
  right_hand%get_array_ptr(3,1) = right_hand%get_array_ptr(3,1) + 1d0
  right_hand%get_array_ptr(3,2) = right_hand%get_array_ptr(3,2) + 1d0
  right_hand%get_array_ptr(3,3) = right_hand%get_array_ptr(3,3) + 1d0
  right_hand%get_array_ptr(3,4) = right_hand%get_array_ptr(3,4) + 1d0
  right_hand%get_array_ptr(4,1) = right_hand%get_array_ptr(4,1) + 1d0
  right_hand%get_array_ptr(4,2) = right_hand%get_array_ptr(4,2) + 1d0
  right_hand%get_array_ptr(4,3) = right_hand%get_array_ptr(4,3) + 1d0
  right_hand%get_array_ptr(5,1) = right_hand%get_array_ptr(5,1) + 1d0
  right_hand%get_array_ptr(5,2) = right_hand%get_array_ptr(5,2) + 1d0
  right_hand%get_array_ptr(5,3) = right_hand%get_array_ptr(5,3) + 1d0
  right_hand%get_array_ptr(6,1) = right_hand%get_array_ptr(6,1) + 1d0
  right_hand%get_array_ptr(6,2) = right_hand%get_array_ptr(6,2) + 1d0
  
  call start_array_tests("array_ptr:supernode")
  call add_test("node=1,index=1", right_hand%get_supernode_ptr(1,1), [(1d0, i=1,3)])
  call add_test("node=1,index=2", right_hand%get_supernode_ptr(1,2), [(0d0, i=1,2)])
  call add_test("node=2,index=1", right_hand%get_supernode_ptr(2,1), [(1d0, i=1,3)])
  call add_test("node=2,index=2", right_hand%get_supernode_ptr(2,2), [(1d0, i=1,3)])
  call add_test("node=3,index=1", right_hand%get_supernode_ptr(3,1), [(1d0, i=1,3)])
  call add_test("node=3,index=2", right_hand%get_supernode_ptr(3,2), [(1d0, i=1,3)])
  call add_test("node=3,index=3", right_hand%get_supernode_ptr(3,3), [(0d0, i=1,1)])
  call add_test("node=4,index=1", right_hand%get_supernode_ptr(4,1), [(1d0, i=1,3)])
  call add_test("node=4,index=2", right_hand%get_supernode_ptr(4,2), [(0d0, i=1,2)])
  call add_test("node=5,index=1", right_hand%get_supernode_ptr(5,1), [(1d0, i=1,3)])
  call add_test("node=6,index=1", right_hand%get_supernode_ptr(6,1), [(1d0, i=1,3)])
  call add_test("node=6,index=2", right_hand%get_supernode_ptr(6,2), [(1d0, i=1,3)])
  call end_array_tests()

  call start_array_tests("array_ptr:border")
  call add_test("node=1,index=2", right_hand%get_border_ptr(1,2), [(1d0, i=1,3)])
  call add_test("node=3,index=3", right_hand%get_border_ptr(3,3), [(1d0, i=1,3)])
  call add_test("node=4,index=2", right_hand%get_border_ptr(4,2), [(1d0, i=1,3)])
  call end_array_tests

  call start_array_tests("array_ptr:work")
  call add_test("node=1,index=2", right_hand%get_work_ptr(1,2), [(0d0, i=1,1)])
  call add_test("node=1,index=3", right_hand%get_work_ptr(1,3), [(1d0, i=1,3)])
  call add_test("node=1,index=4", right_hand%get_work_ptr(1,4), [(1d0, i=1,1)])
  call add_test("node=2,index=3", right_hand%get_work_ptr(2,3), [(1d0, i=1,3)])
  call add_test("node=2,index=4", right_hand%get_work_ptr(2,4), [(1d0, i=1,1)])
  call add_test("node=3,index=3", right_hand%get_work_ptr(3,3), [(0d0, i=1,2)])
  call add_test("node=3,index=4", right_hand%get_work_ptr(3,4), [(1d0, i=1,2)])
  call add_test("node=4,index=2", right_hand%get_work_ptr(4,2), [(0d0, i=1,1)])
  call add_test("node=4,index=3", right_hand%get_work_ptr(4,3), [(1d0, i=1,3)])
  call add_test("node=5,index=2", right_hand%get_work_ptr(5,2), [(1d0, i=1,3)])
  call add_test("node=5,index=3", right_hand%get_work_ptr(5,3), [(1d0, i=1,3)])
  call end_array_tests


end program