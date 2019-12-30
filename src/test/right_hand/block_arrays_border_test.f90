program block_arrays_border_test
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
  node_data => create_node_data([1,6,4], [4,5,0], nb)
  block_arrays => create_block_arrays(node_data, BORDER_EXTRACTOR)

  ptr => block_arrays%get_ptr(1,1); ptr = 0.0d0
  ptr => block_arrays%get_ptr(1,1); ptr = ptr + 1.0d0
  call start_array_tests("double access & size:test1")
  call add_test("(node, index) = (1,1)", block_arrays%get_ptr(1,1), [(1d0, i=1,3)])
  call end_array_tests()

  nb = 3
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  block_arrays => create_block_arrays(node_data, BORDER_EXTRACTOR)

  ptr => block_arrays%get_ptr(1,2); ptr = 0d0
  ptr => block_arrays%get_ptr(3,3); ptr = 0d0
  ptr => block_arrays%get_ptr(4,2); ptr = 0d0
  
  ptr => block_arrays%get_ptr(1,2); ptr = ptr + 1d0
  ptr => block_arrays%get_ptr(3,3); ptr = ptr + 1d0
  ptr => block_arrays%get_ptr(4,2); ptr = ptr + 1d0
  
  call start_array_tests("double access & size:test2")
  call add_test("(1,2)", block_arrays%get_ptr(1,2), [(1d0, i=1,3)])
  call add_test("(3,3)", block_arrays%get_ptr(3,3), [(1d0, i=1,3)])
  call add_test("(4,2)", block_arrays%get_ptr(4,2), [(1d0, i=1,3)])
  call end_array_tests()

end program