module analyze_phase_m
  use jagged_array_m
  use doubly_linked_lists_m
  use ccs_to_crs_m
  use compute_tree_m
  use tree_m
  use reordering_m
  use perm_m
  use finding_leaves_m
  use fundamental_supernode_m
  use relax_supernode_m
  use column_count_m
  use relaxed_supernode_m
  implicit none
  private
  
contains
  subroutine analyze_phase(origin_ccs, max_zero)
    ! NOTE: reorderingは外部で行う
    type(jagged_array_c), intent(in) :: origin_ccs
    integer, intent(in) :: max_zero
    type(jagged_array_c) :: origin_crs, tree_child, postordering_ccs
    integer, pointer, contiguous :: parent(:), postordering_parent(:), postordering_perm(:), postordering_iperm(:)
    integer, pointer, contiguous :: subtree_size(:), isleaf(:)
    integer, pointer, contiguous :: first_node(:), last_node(:), num_child_supernode(:), parent_supernode(:)
    integer, pointer, contiguous :: cc_node(:), cc_supernode(:)
    type(jagged_array_c) :: ccs_supernode, tree_child_supernode, ccs_supernode2, ccs_relaxed
    type(doubly_linked_lists_c) :: merge_lists, son_lists
    integer, pointer, contiguous :: map(:), first_node_relaxed(:)
    integer :: order
    order = origin_ccs%get_num_arrays()
    call ccs_to_crs(origin_ccs, origin_crs)
    parent => compute_tree(origin_crs)
    tree_child = create_tree_child(parent)
    postordering_perm => tree_traverse_postordering(tree_child)
    call set_iperm(postordering_perm, postordering_iperm)
    postordering_ccs = reordering_ccs(origin_ccs, postordering_perm, postordering_iperm)
    postordering_parent => reordering_tree(parent, postordering_perm, postordering_iperm)
    ! TODO: finalize tree_child
    tree_child = create_tree_child(postordering_parent)
    
    ! finding supernode
    subtree_size => count_subtree_size(tree_child)
    isleaf => finding_leaves(subtree_size, postordering_ccs)
    first_node => search_first_node_in_supernode(isleaf, tree_child)
    ccs_supernode = create_supernodal_ccs(first_node, postordering_ccs)
    num_child_supernode => create_supernodal_tree(first_node, tree_child)
    parent_supernode => create_parent_in_postordering_tree(num_child_supernode)
    tree_child_supernode = create_tree_child(num_child_supernode, parent_supernode)
    ! column count
    cc_node => column_count(postordering_ccs, tree_child, postordering_parent)
    cc_supernode => create_supernodal_column_count(first_node, cc_node)
    
    ! relax supernode
    merge_lists = compute_merge_lists(cc_supernode, tree_child_supernode, first_node, max_zero)
    map => build_map(merge_lists)
    first_node_relaxed => create_first_node(first_node, map, merge_lists)
    ! TODO: ccs_supernode2, map2, merge_lists2 by reordering ccs_supernode
    ccs_relaxed = create_ccs(map, merge_lists, first_node_relaxed, ccs_supernode2, order)
    ! symbolic

    ! local index
    ! reordering-ccs_value

    
    

  end subroutine
end module