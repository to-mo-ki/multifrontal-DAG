program create_supernodal_index_test
  use create_supernodal_index_m
  use contiguous_sets_m
  use jagged_array_m
  use test_util
  implicit none
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: l_structure, a_structure, supernodal_index
  integer :: nb
  
  node_sets => create_contiguous_sets([2, 2, 2, 3])
  l_structure => create_jagged_array([2, 2, 2, 0], [8, 9, 7, 9, 7, 8])
  nb = 2
  a_structure => create_jagged_array([3,2,4,2,3,3,2,1,1],[1,2,8,2,9,3,4,7,9,4,9,5,6,8,6,7,8,7,9,8,9])
  supernodal_index => create_supernodal_index(node_sets, a_structure, l_structure)
  call assert_equal("supernodal_index", supernodal_index%get_val(), [1,2,3,2,4,1,2,3,4,2,4,1,2,4,2,3,4,1,3,2,3])
  
end program create_supernodal_index_test