module hppllt_data_m
  use node_data_m
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use factors_m
  use block_local_index_info_m
  use right_hand_m
  use ccs_m
  use jagged_array_cptr_m
  use starpu_factors_m
  implicit none
  
  type(node_data_c), pointer :: node_data
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: a_structure, origin_structure
  type(ccs_c), pointer :: ccs
  type(factors_c), pointer :: factors
  type(block_local_index_info_c), pointer :: block_local_index_info
  type(jagged_array_3D_c), pointer :: block_local_index
  type(jagged_array_c), pointer :: supernodal_index
  type(right_hand_c), pointer :: rh
  integer, pointer, contiguous :: parent(:)
  integer, pointer, contiguous :: perm(:)
  type(jagged_array_c), pointer :: tree_child
  integer, pointer, contiguous :: ccs_perm(:)

  type(jagged_array_cptr_c), pointer :: starpu_block_local_index
  type(starpu_factors_c), pointer :: starpu_factors

end module