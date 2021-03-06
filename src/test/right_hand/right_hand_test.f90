program right_hand_test
  use right_hand_m
  use test_util
  use node_data_m
  implicit none
  type(right_hand_c), pointer :: right_hand
  type(node_data_c), pointer :: node_data
  integer :: nb, i
  double precision, pointer, contiguous :: ptr(:)

  print *, "test1"
  nb = 3
  node_data => create_node_data([1,6,4], [4,5,0], nb)
  right_hand => create_right_hand(node_data, nb)

  ptr => right_hand%get_supernode(1,1); ptr = 0.0d0
  ptr => right_hand%get_supernode(2,1); ptr = 0.0d0
  ptr => right_hand%get_supernode(2,2); ptr = 0.0d0
  ptr => right_hand%get_supernode(3,1); ptr = 0.0d0
  ptr => right_hand%get_supernode(3,2); ptr = 0.0d0

  ptr => right_hand%get_border(1,1); ptr = 0.0d0

  ptr => right_hand%get_work(1,1); ptr = 0.0d0
  ptr => right_hand%get_work(1,2); ptr = 0.0d0
  ptr => right_hand%get_work(2,3); ptr = 0.0d0
  ptr => right_hand%get_work(2,4); ptr = 0.0d0

  ptr => right_hand%get_array_ptr(1,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(1,2); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,2); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,3); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,4); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(3,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(3,2); ptr = ptr + 1d0

  call start_array_tests("matrix_ptr:supernode")
  call add_test("node=1,index=1", right_hand%get_supernode(1,1), [(0d0, i=1,1)])
  call add_test("node=2,index=1", right_hand%get_supernode(2,1), [(1d0, i=1,3)])
  call add_test("node=2,index=2", right_hand%get_supernode(2,2), [(1d0, i=1,3)])
  call add_test("node=3,index=1", right_hand%get_supernode(3,1), [(1d0, i=1,3)])
  call add_test("node=3,index=2", right_hand%get_supernode(3,2), [(1d0, i=1,1)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:border")
  call add_test("node=1,index=1", right_hand%get_border(1,1), [(1d0, i=1,3)])
  call end_array_tests()

  call start_array_tests("matrix_ptr:work")
  call add_test("node=1,index=1", right_hand%get_work(1,1), [(0d0, i=1,2)])
  call add_test("node=1,index=2", right_hand%get_work(1,2), [(1d0, i=1,2)])
  call add_test("node=2,index=3", right_hand%get_work(2,3), [(1d0, i=1,3)])
  call add_test("node=2,index=4", right_hand%get_work(2,4), [(1d0, i=1,2)])
  call end_array_tests()


  print *, "test2"
  nb = 3
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  right_hand => create_right_hand(node_data, nb)

  ptr => right_hand%get_supernode(1,1); ptr = 0d0
  ptr => right_hand%get_supernode(1,2); ptr = 0d0
  ptr => right_hand%get_supernode(2,1); ptr = 0d0
  ptr => right_hand%get_supernode(2,2); ptr = 0d0
  ptr => right_hand%get_supernode(3,1); ptr = 0d0
  ptr => right_hand%get_supernode(3,2); ptr = 0d0
  ptr => right_hand%get_supernode(3,3); ptr = 0d0
  ptr => right_hand%get_supernode(4,1); ptr = 0d0
  ptr => right_hand%get_supernode(4,2); ptr = 0d0
  ptr => right_hand%get_supernode(5,1); ptr = 0d0
  ptr => right_hand%get_supernode(6,1); ptr = 0d0
  ptr => right_hand%get_supernode(6,2); ptr = 0d0

  ptr => right_hand%get_border(1,2); ptr = 0d0
  ptr => right_hand%get_border(3,3); ptr = 0d0
  ptr => right_hand%get_border(4,2); ptr = 0d0

  ptr => right_hand%get_work(1,2); ptr = 0d0
  ptr => right_hand%get_work(1,3); ptr = 0d0
  ptr => right_hand%get_work(1,4); ptr = 0d0
  ptr => right_hand%get_work(2,3); ptr = 0d0
  ptr => right_hand%get_work(2,4); ptr = 0d0
  ptr => right_hand%get_work(3,3); ptr = 0d0
  ptr => right_hand%get_work(3,4); ptr = 0d0
  ptr => right_hand%get_work(4,2); ptr = 0d0
  ptr => right_hand%get_work(4,3); ptr = 0d0
  ptr => right_hand%get_work(5,2); ptr = 0d0
  ptr => right_hand%get_work(5,3); ptr = 0d0

  ptr => right_hand%get_array_ptr(1,1); ptr = ptr + 1d0 
  ptr => right_hand%get_array_ptr(1,2); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(1,3); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(1,4); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,2); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,3); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(2,4); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(3,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(3,2); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(3,3); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(3,4); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(4,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(4,2); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(4,3); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(5,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(5,2); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(5,3); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(6,1); ptr = ptr + 1d0
  ptr => right_hand%get_array_ptr(6,2); ptr = ptr + 1d0
  
  call start_array_tests("array_ptr:supernode")
  call add_test("node=1,index=1", right_hand%get_supernode(1,1), [(1d0, i=1,3)])
  call add_test("node=1,index=2", right_hand%get_supernode(1,2), [(0d0, i=1,2)])
  call add_test("node=2,index=1", right_hand%get_supernode(2,1), [(1d0, i=1,3)])
  call add_test("node=2,index=2", right_hand%get_supernode(2,2), [(1d0, i=1,3)])
  call add_test("node=3,index=1", right_hand%get_supernode(3,1), [(1d0, i=1,3)])
  call add_test("node=3,index=2", right_hand%get_supernode(3,2), [(1d0, i=1,3)])
  call add_test("node=3,index=3", right_hand%get_supernode(3,3), [(0d0, i=1,1)])
  call add_test("node=4,index=1", right_hand%get_supernode(4,1), [(1d0, i=1,3)])
  call add_test("node=4,index=2", right_hand%get_supernode(4,2), [(0d0, i=1,2)])
  call add_test("node=5,index=1", right_hand%get_supernode(5,1), [(1d0, i=1,3)])
  call add_test("node=6,index=1", right_hand%get_supernode(6,1), [(1d0, i=1,3)])
  call add_test("node=6,index=2", right_hand%get_supernode(6,2), [(1d0, i=1,3)])
  call end_array_tests()

  call start_array_tests("array_ptr:border")
  call add_test("node=1,index=2", right_hand%get_border(1,2), [(1d0, i=1,3)])
  call add_test("node=3,index=3", right_hand%get_border(3,3), [(1d0, i=1,3)])
  call add_test("node=4,index=2", right_hand%get_border(4,2), [(1d0, i=1,3)])
  call end_array_tests

  call start_array_tests("array_ptr:work")
  call add_test("node=1,index=2", right_hand%get_work(1,2), [(0d0, i=1,1)])
  call add_test("node=1,index=3", right_hand%get_work(1,3), [(1d0, i=1,3)])
  call add_test("node=1,index=4", right_hand%get_work(1,4), [(1d0, i=1,1)])
  call add_test("node=2,index=3", right_hand%get_work(2,3), [(1d0, i=1,3)])
  call add_test("node=2,index=4", right_hand%get_work(2,4), [(1d0, i=1,1)])
  call add_test("node=3,index=3", right_hand%get_work(3,3), [(0d0, i=1,2)])
  call add_test("node=3,index=4", right_hand%get_work(3,4), [(1d0, i=1,2)])
  call add_test("node=4,index=2", right_hand%get_work(4,2), [(0d0, i=1,1)])
  call add_test("node=4,index=3", right_hand%get_work(4,3), [(1d0, i=1,3)])
  call add_test("node=5,index=2", right_hand%get_work(5,2), [(1d0, i=1,3)])
  call add_test("node=5,index=3", right_hand%get_work(5,3), [(1d0, i=1,3)])
  call end_array_tests


end program