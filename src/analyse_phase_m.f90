module analyze_phase_m
  use jagged_array_m
  use ccs_to_crs_m
  use compute_tree_m
  use tree_m
  use reordering_m
  use perm_m
  implicit none
  private
  
contains
  subroutine analyze_phase(origin_ccs)
    ! NOTE: reorderingは外部で行う
    type(jagged_array_c), intent(in) :: origin_ccs
    type(jagged_array_c) :: origin_crs, tree_child, postordering_ccs
    integer, pointer, contiguous :: parent(:), postordering_parent(:), postordering_perm(:), postordering_iperm

    call ccs_to_crs(origin_ccs, origin_crs)
    parent => compute_tree(origin_crs)
    tree_child => create_tree_child(parent)
    postordering_perm => tree_traverse_postordering(tree_child)
    call set_iperm(postordering_perm, postordering_iperm)
    postordering_ccs = reordering_ccs(origin_ccs, postordering_perm, postordering_iperm)
    postordering_parent => reordering_tree(parent, postordering_perm, postordering_iperm)
    
    

  end subroutine
end module