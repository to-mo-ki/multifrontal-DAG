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
  use supernode_m
  implicit none
  private
  
contains
  subroutine analyze_phase(origin_ccs, max_zero, perm_out)
    ! NOTE: reorderingは外部で行う
    type(jagged_array_c), pointer, intent(in) :: origin_ccs
    integer, intent(in) :: max_zero
    type(jagged_array_c), pointer :: origin_crs, tree_child, ccs_l, ccs_supernode, postordering_ccs
    integer, pointer, contiguous :: parent(:), postordering_parent(:)
    integer, pointer, contiguous :: cc_node(:), postordering_perm(:), postordering_iperm(:)
    type(supernode_c), pointer :: fundamental, relaxed
    integer, pointer, contiguous :: perm_out(:)
    
    allocate(fundamental, relaxed)
    call ccs_to_crs(origin_ccs, origin_crs)
    
    parent => compute_tree(origin_crs)
    tree_child => create_tree_child(parent)

    postordering_perm => tree_traverse_postordering(tree_child)
    call set_iperm(postordering_perm, postordering_iperm)
    postordering_ccs => reordering_ccs(origin_ccs, postordering_perm, postordering_iperm)
    postordering_parent => reordering_tree(parent, postordering_perm, postordering_iperm)
    ! TODO: finalize tree_child
    tree_child => create_tree_child(postordering_parent)
    ! column count
    cc_node => column_count(postordering_ccs, tree_child, postordering_parent)
    
    ! finding supernode
    fundamental%ccs => postordering_ccs
    fundamental%perm => postordering_perm
    fundamental%iperm => postordering_iperm
    call finding_supernode(tree_child, cc_node, fundamental)
    
    
    ! relax supernode
    call relax_supernode(fundamental, relaxed, max_zero)
    
    ccs_supernode => create_supernodal_ccs(relaxed%node_sets, relaxed%ccs)

    ccs_l => symbolic_factorize(ccs_supernode, relaxed%node_sets, relaxed%cc, relaxed%tree_child)

    ! Tree pruning
    ! local index
    ! CCSのオーダリングはpermを用いて行う(ccs_permを作成しない)
    call perm_product(fundamental%perm, relaxed%perm, perm_out)


    
    

  end subroutine

  subroutine finding_supernode(tree_child, cc, fundamental)
    use finding_leaves_m
    use fundamental_supernode_m
    type(jagged_array_c), pointer, intent(in) :: tree_child
    integer, pointer, contiguous, intent(in) :: cc(:)
    type(supernode_c), pointer, intent(inout) :: fundamental
    integer, pointer, contiguous :: subtree_size(:), isleaf(:), num_child_supernode(:), parent_supernode(:)

    subtree_size => count_subtree_size(tree_child)
    isleaf => finding_leaves(subtree_size, fundamental%ccs)
    fundamental%node_sets => search_node_sets_in_supernode(isleaf, tree_child)
    num_child_supernode => create_supernodal_tree(fundamental%node_sets, tree_child)
    parent_supernode => create_parent_in_postordering_tree(num_child_supernode)
    fundamental%tree_child => create_tree_child(num_child_supernode, parent_supernode)
    fundamental%cc => create_supernodal_column_count(fundamental%node_sets, cc)
   
  end subroutine

  subroutine relax_supernode(fundamental, relaxed, max_zero)
    type(supernode_c), intent(in) :: fundamental
    type(supernode_c), intent(out) :: relaxed
    integer, intent(in) :: max_zero
    type(doubly_linked_lists_c), pointer :: merge_lists, sons
    integer, pointer, contiguous :: map(:), num_child(:), parent(:)

    merge_lists => compute_merge_lists(fundamental%cc, fundamental%tree_child, fundamental%node_sets, max_zero, sons)
    map => build_map(merge_lists)
    relaxed%node_sets => create_node_sets(fundamental%node_sets, map, merge_lists)
    relaxed%perm => create_perm(fundamental%node_sets, relaxed%node_sets, map, merge_lists)
    num_child => count_num_child(sons, map)
    parent => create_parent_in_postordering_tree(num_child)
    relaxed%tree_child => create_tree_child(num_child, parent)
    relaxed%cc => build_cc(fundamental%cc, map)
    call set_iperm(relaxed%perm, relaxed%iperm)
    relaxed%ccs = reordering_ccs(fundamental%ccs, relaxed%perm, relaxed%iperm)
  end subroutine
end module