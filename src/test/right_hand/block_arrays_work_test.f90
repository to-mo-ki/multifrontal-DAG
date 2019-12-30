program block_arrays_work_test
  use block_arrays_m
  use node_data_m
  use extractor_types_m
  use test_util
  implicit none
  type(block_arrays_c), pointer :: block_arrays
  type(node_data_c), pointer :: node_data
  integer :: nb, i
  double precision, pointer, contiguous :: ptr(:)

  nb = 3
  node_data => create_node_data([1, 6, 4], [4, 5, 0], nb)
  block_arrays => create_block_arrays(node_data, WORK_EXTRACTOR)

  ptr => block_arrays%get_ptr(1,1); ptr = 0.0d0
  ptr => block_arrays%get_ptr(1,2); ptr = 0.0d0
  ptr => block_arrays%get_ptr(2,3); ptr = 0.0d0
  ptr => block_arrays%get_ptr(2,4); ptr = 0.0d0

  ptr => block_arrays%get_ptr(1,1); ptr = ptr + 1.0d0
  ptr => block_arrays%get_ptr(1,2); ptr = ptr + 1.0d0
  ptr => block_arrays%get_ptr(2,3); ptr = ptr + 1.0d0
  ptr => block_arrays%get_ptr(2,4); ptr = ptr + 1.0d0

  call start_array_tests("double access & size:test1")
  call add_test("(1,1)", block_arrays%get_ptr(1,1),[(1d0, i=1,2)])
  call add_test("(1,2)", block_arrays%get_ptr(1,2),[(1d0, i=1,2)])
  call add_test("(2,3)", block_arrays%get_ptr(2,3),[(1d0, i=1,3)])
  call add_test("(2,4)", block_arrays%get_ptr(2,4),[(1d0, i=1,2)])
  call end_array_tests()

  nb = 3
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  block_arrays => create_block_arrays(node_data, WORK_EXTRACTOR)

  ptr => block_arrays%get_ptr(1,2); ptr = 0d0
  ptr => block_arrays%get_ptr(1,3); ptr = 0d0
  ptr => block_arrays%get_ptr(1,4); ptr = 0d0
  ptr => block_arrays%get_ptr(2,3); ptr = 0d0
  ptr => block_arrays%get_ptr(2,4); ptr = 0d0
  ptr => block_arrays%get_ptr(3,3); ptr = 0d0
  ptr => block_arrays%get_ptr(3,4); ptr = 0d0
  ptr => block_arrays%get_ptr(4,2); ptr = 0d0
  ptr => block_arrays%get_ptr(4,3); ptr = 0d0
  ptr => block_arrays%get_ptr(5,2); ptr = 0d0
  ptr => block_arrays%get_ptr(5,3); ptr = 0d0
  

  ptr => block_arrays%get_ptr(1,2); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(1,3); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(1,4); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(2,3); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(2,4); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(3,3); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(3,4); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(4,2); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(4,3); ptr = ptr +1d0
  ptr => block_arrays%get_ptr(5,2); ptr = ptr + 1d0
  ptr => block_arrays%get_ptr(5,3); ptr = ptr + 1d0
  
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