program relaxed_supernode_test
  use jagged_array_m
  use doubly_linked_lists_m
  use contiguous_sets_m
  use relaxed_supernode_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs_fundamental, ccs_relaxed
  type(doubly_linked_lists_c) :: merge_lists
  integer, pointer, contiguous :: map(:), first_node(:), perm(:)
  type(contiguous_sets_c) :: node_sets_relaxed, node_sets_fundamental
  integer, pointer, contiguous :: num_row(:), row(:)
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

  node_sets_fundamental = create_contiguous_sets((/1, 1, 2, 1, 1, 1, 2/))
  node_sets_relaxed = create_node_sets(node_sets_fundamental, map, merge_lists)
  allocate(first_node(5))
  do i=1, 5
    first_node(i) = node_sets_relaxed%get_first(i)
  enddo
  call assert_equal("max_zero=1:create node sets", first_node, (/1, 3, 5, 7, 10/))
  
  perm => create_perm(node_sets_fundamental, node_sets_relaxed, map, merge_lists)
  call assert_equal("max_zero=1:perm", perm, (/2, 1, 3, 4, 6, 5, 8, 9, 7/))

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

  node_sets_relaxed = create_node_sets(node_sets_fundamental, map, merge_lists)
  allocate(first_node(4))
  do i=1, 4
    first_node(i) = node_sets_relaxed%get_first(i)
  enddo
  call assert_equal("max_zero=2:create node sets", first_node, (/1, 3, 6, 10/))
  
  perm => create_perm(node_sets_fundamental, node_sets_relaxed, map, merge_lists)
  call assert_equal("max_zero=2:perm", perm, (/6, 5, 7, 3, 4, 8, 9, 2, 1/))
  
end program relaxed_supernode_test