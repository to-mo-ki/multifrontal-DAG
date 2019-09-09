module analyze_phase_m
  use jagged_array_m
  use ccs_to_crs_m
  use compute_tree_m
  use tree_m
  use reordering_m
  use perm_m
  use finding_leaves_m
  use supernode_m
  use column_count_m
  implicit none
  private
  
contains
  subroutine analyze_phase(origin_ccs)
    ! NOTE: reorderingは外部で行う
    type(jagged_array_c), intent(in) :: origin_ccs
    type(jagged_array_c) :: origin_crs, tree_child, postordering_ccs
    integer, pointer, contiguous :: parent(:), postordering_parent(:), postordering_perm(:), postordering_iperm(:)
    integer, pointer, contiguous :: subtree_size(:), isleaf(:)
    integer, pointer, contiguous :: first_node(:), last_node(:), num_child_supernode(:), parent_supernode(:)
    integer, pointer, contiguous :: cc(:)
    type(jagged_array_c) :: ccs_supernode, tree_child_supernode
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
    !ccs_supernode = create_supernodal_ccs(first_node, postordering_ccs)
    !num_child_supernode => create_supernodal_tree(first_node, tree_child)
    !parent_supernode => create_parent_in_postordering_tree(num_child_supernode)
    !tree_child_supernode = create_tree_child(num_child_supernode, parent_supernode)

    cc => column_count(postordering_ccs, tree_child, postordering_parent)
    
    ! relax supernode
    ! symbolic

    ! local index
    ! reordering-ccs_value

    
    

  end subroutine
end module