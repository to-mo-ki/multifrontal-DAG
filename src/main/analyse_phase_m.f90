module analyze_phase_m
  use jagged_array_m
  use doubly_linked_lists_m
  use contiguous_sets_m
  use ccs_to_crs_m
  use compute_tree_m
  use symbolic_factorize_m
  use tree_m
  use reordering_m
  use perm_m
  use relax_supernode_m
  use column_count_m
  use relaxed_supernode_m
  use fundamental_supernode_m
  implicit none
  private
  
contains
  subroutine analyze_phase(origin_ccs, max_zero)
    ! NOTE: reorderingは外部で行う
    type(jagged_array_c), intent(in) :: origin_ccs
    integer, intent(in) :: max_zero
    type(jagged_array_c) :: origin_crs, tree_child, postordering_ccs, ccs_l
    integer, pointer, contiguous :: parent(:), postordering_parent(:), postordering_perm(:), postordering_iperm(:)
    type(contiguous_sets_c) :: node_sets, node_sets_relaxed
    integer, pointer, contiguous :: cc_node(:), cc_fundamental(:)
    type(jagged_array_c) :: ccs_supernode, tree_child_fundamental, ccs_supernode2, ccs_relaxed
    type(doubly_linked_lists_c) :: merge_lists, son_lists
    integer, pointer, contiguous :: map(:)
    integer, pointer, contiguous :: relaxed_perm(:), relaxed_iperm(:)
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
    call finding_supernode(tree_child, postordering_ccs, tree_child_fundamental, node_sets)
    ! column count
    cc_node => column_count(postordering_ccs, tree_child, postordering_parent)
    cc_fundamental => create_supernodal_column_count(node_sets, cc_node)
    
    ! relax supernode
    call relax_supernode(cc_fundamental, tree_child_fundamental, node_sets, max_zero, node_sets_relaxed, relaxed_perm)
    call set_iperm(relaxed_perm, relaxed_iperm)
    ccs_relaxed = reordering_ccs(postordering_ccs, relaxed_perm, relaxed_iperm)

    ccs_supernode = create_supernodal_ccs(node_sets_relaxed, ccs_relaxed)

    ! symbolic
    ! TODO: cc_relaxed, tree_childを求める
    !ccs_l = symbolic_factorize(ccs_supernode, node_sets_relaxed, cc_relaxed, tree_child_relaxed)

    ! local index
    ! reordering-ccs_value

    
    

  end subroutine

  subroutine finding_supernode(tree_child_node, ccs_node, tree_child_fundamental, node_sets)
    use finding_leaves_m
    use fundamental_supernode_m
    type(jagged_array_c), intent(in) :: tree_child_node, ccs_node
    type(jagged_array_c), intent(out) :: tree_child_fundamental
    type(contiguous_sets_c), intent(out) :: node_sets
    integer, pointer, contiguous :: subtree_size(:), isleaf(:), num_child_supernode(:), parent_supernode(:)

    subtree_size => count_subtree_size(tree_child_node)
    isleaf => finding_leaves(subtree_size, ccs_node)
    node_sets = search_node_sets_in_supernode(isleaf, tree_child_node)
    num_child_supernode => create_supernodal_tree(node_sets, tree_child_node)
    parent_supernode => create_parent_in_postordering_tree(num_child_supernode)
    tree_child_fundamental = create_tree_child(num_child_supernode, parent_supernode)
   
  end subroutine

  subroutine relax_supernode(cc_fundamental, tree_child_fundamental, node_sets, max_zero, node_sets_relaxed, relaxed_perm)
    integer, pointer, contiguous, intent(in) :: cc_fundamental(:)
    type(jagged_array_c), intent(in) :: tree_child_fundamental
    type(contiguous_sets_c), intent(in) :: node_sets
    integer, intent(in) :: max_zero
    integer, pointer, contiguous, intent(out) :: relaxed_perm(:)
    type(contiguous_sets_c), intent(out) :: node_sets_relaxed
    type(doubly_linked_lists_c) :: merge_lists
    integer, pointer, contiguous :: map(:), relaxed_iperm(:)

    merge_lists = compute_merge_lists(cc_fundamental, tree_child_fundamental, node_sets, max_zero)
    map => build_map(merge_lists)
    node_sets_relaxed = create_node_sets(node_sets, map, merge_lists)
    relaxed_perm => create_perm(node_sets, node_sets_relaxed, map, merge_lists)
    
  end subroutine
end module