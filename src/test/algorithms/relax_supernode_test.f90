program relax_supernode_test
  ! TODO: assert sons
  use jagged_array_m
  use doubly_linked_lists_m
  use contiguous_sets_m
  use iterator_m
  use relax_supernode_m
  use sparse_matrix_maker_m
  use tree_maker_m
  use test_util
  implicit none
  
  type(doubly_linked_lists_c), pointer :: merge_lists, sons
  type(jagged_array_c), pointer :: tree_child
  integer, pointer, contiguous :: cc(:), num_child(:), child_val(:), node_size(:)
  type(contiguous_sets_c), pointer :: node_sets

  call make_supernodal_ccs(cc, node_size=node_size)
  call make_supernodal_tree(num_child, child_val)

  tree_child => create_jagged_array(num_child, child_val)
  node_sets => create_contiguous_sets(node_size)
  merge_lists => compute_merge_lists(cc, tree_child, node_sets, 0, sons)
  call assert_equal("max_zero=0:1", get_merge_nodes(1), [1])
  call assert_equal("max_zero=0:2", get_merge_nodes(2), [2])
  call assert_equal("max_zero=0:3", get_merge_nodes(3), [3])
  call assert_equal("max_zero=0:4", get_merge_nodes(4), [4])
  call assert_equal("max_zero=0:5", get_merge_nodes(5), [5])
  call assert_equal("max_zero=0:7", get_merge_nodes(7), [7, 6])

  merge_lists => compute_merge_lists(cc, tree_child, node_sets, 1, sons)
  call assert_equal("max_zero=1:2", get_merge_nodes(2), [2, 1])
  call assert_equal("max_zero=1:3", get_merge_nodes(3), [3])
  call assert_equal("max_zero=1:5", get_merge_nodes(5), [5, 4])
  call assert_equal("max_zero=1:7", get_merge_nodes(7), [7, 6])
  
  merge_lists => compute_merge_lists(cc, tree_child, node_sets, 2, sons)
  call assert_equal("max_zero=2:5", get_merge_nodes(5), [5, 4])
  call assert_equal("max_zero=2:6", get_merge_nodes(6), [6, 3])
  call assert_equal("max_zero=2:7", get_merge_nodes(7), [7, 2, 1])

contains
  function get_merge_nodes(node) result(merge_nodes)
    integer, intent(in) :: node
    type(iterator_c), pointer :: iterator
    integer, pointer, contiguous :: merge_nodes(:)
    integer :: ptr

    allocate(merge_nodes(9))
    iterator => merge_lists%create_iterator(node)
    ptr = 0
    do while(iterator%has_next())
      ptr = ptr + 1
      merge_nodes(ptr) = iterator%next()
    enddo
    
    merge_nodes => merge_nodes(1:ptr)
    
  end function
  
  
end program relax_supernode_test