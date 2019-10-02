program relaxed_supernode_test
  use jagged_array_m
  use doubly_linked_lists_m
  use contiguous_sets_m
  use relaxed_supernode_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(doubly_linked_lists_c), pointer :: merge_lists, sons
  integer, pointer, contiguous :: map(:), first_node(:), perm(:), num_child(:)
  type(contiguous_sets_c), pointer :: node_sets_relaxed, node_sets_fundamental
  integer, pointer, contiguous :: cc_fundamental(:), cc_relaxed(:), node_size(:)
  integer :: i

  merge_lists => create_doubly_linked_lists(7)
  do i=1, 7
    call merge_lists%add(i, i)
  enddo
  call merge_lists%merge(1, 2)
  call merge_lists%merge(4, 5)
  call merge_lists%merge(6, 7)

  map => build_map(merge_lists)
  call assert_equal("max_zero=1:build map", map, (/2, 3, 5, 7/))
  call make_supernodal_ccs(node_size=node_size)
  node_sets_fundamental => create_contiguous_sets(node_size)
  node_sets_relaxed => create_node_sets(node_sets_fundamental, map, merge_lists)
  allocate(first_node(5))
  do i=1, 5
    first_node(i) = node_sets_relaxed%get_first(i)
  enddo
  call assert_equal("max_zero=1:create node sets", first_node, (/1, 3, 5, 7, 10/))
  
  perm => create_perm(node_sets_fundamental, node_sets_relaxed, map, merge_lists)
  call assert_equal("max_zero=1:perm", perm, (/1, 2, 3, 4, 5, 6, 7, 8, 9/))

  allocate(cc_fundamental(7))
  call make_supernodal_ccs(cc_fundamental)
  cc_relaxed => build_cc(cc_fundamental, map)
  call assert_equal("max_zero=2:cc", cc_relaxed, (/2, 2, 2, 0/))

  sons => create_doubly_linked_lists(7)
  call sons%add(2, 7)
  call sons%add(3, 7)
  call sons%add(5, 7)

  num_child => count_num_child(sons, map)
  call assert_equal("max_zero=1:count num child", num_child, (/0, 0, 0, 3/))

  merge_lists => create_doubly_linked_lists(7)
  do i=1, 7
    call merge_lists%add(i, i)
  enddo
  call merge_lists%merge(1, 2)
  call merge_lists%merge(4, 5)
  call merge_lists%merge(3, 6)
  call merge_lists%merge(2, 7)

  map => build_map(merge_lists)
  call assert_equal("max_zero=2:build map", map, (/5, 6, 7/))

  node_sets_relaxed => create_node_sets(node_sets_fundamental, map, merge_lists)
  allocate(first_node(4))
  do i=1, 4
    first_node(i) = node_sets_relaxed%get_first(i)
  enddo
  call assert_equal("max_zero=2:create node sets", first_node, (/1, 3, 6, 10/))
  
  perm => create_perm(node_sets_fundamental, node_sets_relaxed, map, merge_lists)
  call assert_equal("max_zero=2:perm", perm, (/5, 6, 3, 4, 7, 1, 2, 8, 9/))

  cc_relaxed => build_cc(cc_fundamental, map)
  call assert_equal("max_zero=2:cc", cc_relaxed, (/2, 2, 0/))

  sons => create_doubly_linked_lists(7)
  call sons%add(5, 6)
  call sons%add(6, 7)
  num_child => count_num_child(sons, map)
  call assert_equal("max_zero=2:count num child", num_child, (/0, 1, 1/))

  
end program relaxed_supernode_test