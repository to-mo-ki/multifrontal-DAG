program relaxed_supernode_test
  use jagged_array_m
  use doubly_linked_lists_m
  use relaxed_supernode_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs_fundamental, ccs_relaxed
  type(doubly_linked_lists_c) :: merge_lists
  integer, pointer, contiguous :: map(:), first_node_relaxed(:), first_node_fundamental(:)
  integer, pointer, contiguous :: col(:), row(:)
  integer :: i

  merge_lists = create_doubly_linked_lists(7)
  do i=1, 7
    call merge_lists%add(i, i)
  enddo
  call merge_lists%merge(1, 2)
  call merge_lists%merge(4, 5)
  call merge_lists%merge(6, 7)

  map => build_map(merge_lists)
  call assert_equal("max_zero=1:build map", map, (/2, 3, 5, 7/))

  allocate(first_node_fundamental(8))
  first_node_fundamental = (/1, 2, 3, 5, 6, 7, 8, 10/)
  first_node_relaxed => create_first_node(first_node_fundamental, map, merge_lists)
  call assert_equal("max_zero=1:create first node", first_node_relaxed, (/1, 3, 5, 7, 10/))

  allocate(col(8), row(10))
  col = (/1, 3, 4, 6, 8, 10, 11, 11/)
  row = (/2, 8, 9, 7, 9, 6, 8, 7, 8, 9/)
  ccs_fundamental = create_jagged_array(col, row)
  ccs_relaxed = create_ccs(map, merge_lists, first_node_relaxed, ccs_fundamental, 9)

  call assert_equal("max_zero=1:relaxed ccs(num_array)", ccs_relaxed%get_num_arrays(), 4)
  call assert_equal("max_zero=1:relaxed ccs(1)", ccs_relaxed%get_array(1), (/8, 9/))
  call assert_equal("max_zero=1:relaxed ccs(2)", ccs_relaxed%get_array(2), (/7, 9/))
  call assert_equal("max_zero=1:relaxed ccs(3)", ccs_relaxed%get_array(3), (/7, 8/))
  call assert_equal("max_zero=1:relaxed ccs(4)", ccs_relaxed%get_array_length(4), 0)

  merge_lists = create_doubly_linked_lists(7)
  do i=1, 7
    call merge_lists%add(i, i)
  enddo
  call merge_lists%merge(1, 2)
  call merge_lists%merge(4, 5)
  call merge_lists%merge(3, 6)
  call merge_lists%merge(2, 7)

  map => build_map(merge_lists)
  call assert_equal("max_zero=2:build map", map, (/5, 6, 7/))

  first_node_relaxed => create_first_node(first_node_fundamental, map, merge_lists)
  call assert_equal("max_zero=2:create first node", first_node_relaxed, (/1, 3, 6, 10/))
  allocate(col(8), row(10))
  col = (/1, 3, 5, 7, 8, 10, 11, 11/)
  row = (/2, 8, 5, 8, 5, 9, 9, 6, 7, 9/)
  ccs_fundamental = create_jagged_array(col, row)
  merge_lists = create_doubly_linked_lists(7)
  do i=1, 7
    call merge_lists%add(i, i)
  enddo
  call merge_lists%merge(1, 2)
  call merge_lists%merge(3, 4)
  call merge_lists%merge(5, 6)
  call merge_lists%merge(6, 7)
  allocate(map(3))
  map = (/2, 4, 7/)
  ccs_relaxed = create_ccs(map, merge_lists, first_node_relaxed, ccs_fundamental, 9)
  call assert_equal("max_zero=2:relaxed ccs(num_array)", ccs_relaxed%get_num_arrays(), 3)
  call assert_equal("max_zero=2:relaxed ccs(1)", ccs_relaxed%get_array(1), (/5, 8/))
  call assert_equal("max_zero=2:relaxed ccs(2)", ccs_relaxed%get_array(2), (/9/))
  call assert_equal("max_zero=2:relaxed ccs(3)", ccs_relaxed%get_array_length(3), 0)
  
end program relaxed_supernode_test