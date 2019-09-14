module supernode_m
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  private
  
  type, public :: supernode_c
    type(jagged_array_c), pointer :: ccs, tree_child
    type(contiguous_sets_c), pointer :: node_sets
    integer, pointer, contiguous :: perm(:), iperm(:), cc(:)
  end type

end module