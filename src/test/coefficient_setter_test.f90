program coefficient_setter_test
  use coefficient_setter_m
  use zero_setter_m
  use factors_m
  use jagged_array_m
  use contiguous_sets_m
  use ccs_m
  use test_util
  implicit none
  type(ccs_c), pointer :: ccs
  type(jagged_array_c), pointer :: l_structure, a_structure
  type(contiguous_sets_c), pointer :: node_set
  type(factors_c), pointer :: factors
  double precision, pointer, contiguous :: matrix(:)
  integer :: nb

  node_set => create_contiguous_sets([2, 2, 2, 3])
  l_structure => create_jagged_array([2, 2, 2, 0], [8, 9, 7, 9, 7, 8])
  nb = 2
  factors => create_factors(node_set, l_structure, nb)
  a_structure => create_jagged_array([3,2,4,2,3,3,2,1,1],[1,2,3,2,4,1,2,3,4,2,4,1,2,4,2,3,4,1,3,2,3])
  ccs => create_ccs(a_structure, [double precision :: 11,21,81,22,92,33,43,73,93,44,94,55,65,85,66,76,86,77,97,88,99])
  call set_zero(factors)
  call set_coefficient(ccs, factors, nb)
  matrix => factors%get_supernode_ptr(1,1,1)
  call assert_equal_partial_array("node=1, i=1, j=1", matrix, [1,3,4], 3, [11d0,21d0,22d0])
  matrix => factors%get_supernode_ptr(1,2,1)
  call assert_equal("node=1, i=2, j=1", matrix, [81d0,0d0,0d0,92d0])
  matrix => factors%get_supernode_ptr(2,1,1)
  call assert_equal_partial_array("node=2, i=1, j=1", matrix, [1,3,4], 3, [33d0,43d0,44d0])
  matrix => factors%get_supernode_ptr(2,2,1)
  call assert_equal("node=2, i=2, j=1", matrix, [73d0,0d0,93d0,94d0])
  matrix => factors%get_supernode_ptr(3,1,1)
  call assert_equal_partial_array("node=3, i=1, j=1", matrix, [1,3,4], 3, [55d0,65d0,66d0])
  matrix => factors%get_supernode_ptr(3,2,1)
  call assert_equal("node=3, i=2, j=1", matrix, [0d0,76d0,85d0,86d0])
  matrix => factors%get_supernode_ptr(4,1,1)
  call assert_equal_partial_array("node=4, i=1, j=1", matrix, [1,3,4], 3, [77d0,0d0,88d0])
  matrix => factors%get_supernode_ptr(4,2,1)
  call assert_equal("node=4, i=2, j=1", matrix, [97d0,0d0])
  matrix => factors%get_supernode_ptr(4,2,2)
  call assert_equal("node=4, i=2, j=2", matrix, [99d0])
  
end program coefficient_setter_test