program seq_factorize_test
  use factors_m
  use block_local_index_m
  use jagged_array_m
  use contiguous_sets_m
  use seq_factorize_m
  use test_util
  use coefficient_setter_m
  use ccs_m
  use node_data_m
  implicit none
  type(factors_c), pointer :: factors
  type(block_local_index_c), pointer :: block_local_index
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: local_index, supernodal_index
  type(ccs_c), pointer :: ccs
  type(node_data_c), pointer :: node_data
  double precision, pointer, contiguous :: val(:)
  integer :: nb

  call test_nb_2()
  
contains

  subroutine test_nb_2
    nb = 2
    node_sets => create_contiguous_sets([4,5,4])
    local_index => create_jagged_array([5,3,0],[2,3,4,5,7,1,3,4])
    allocate(val(70), source=[double precision :: 1,2,3,4,1,2,3,4,5,8,12,16,4,8,12,16,20,27,36,9,18,27,36,45,64,16,32,48,64,80,1,2,3,4,5,1,2,3,12,20,28,36,4,28,12,43,60,77,9,58,27,100,128,16,92,48,189,25,130,75,6,2,13,19,8,12,16,147,66,109])
    supernodal_index => create_jagged_array([9,8,7,6,8,7,6,5,4,4,3,2,1],[1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,3,4,5,6,7,8,9,4,5,6,7,8,9,1,2,3,4,5,6,7,8,2,3,4,5,6,7,8,3,4,5,6,7,8,4,5,6,7,8,5,6,7,8,1,2,3,4,2,3,4,3,4,4])
    ccs => create_ccs(supernodal_index, val)
    factors => create_factors(node_sets, local_index, nb)
    node_data => create_node_data([4,5,4],[5,3,0],nb)
    block_local_index => create_block_local_index(node_data, local_index)
    call set_coefficient(ccs, factors, nb)
    call seq_factorize(factors, block_local_index, [2,3,0])

    call start_array_tests("nb=2")
    !call add_test("node=1, (i, j) = (1,1)", factors%get_supernode_ptr(1, 1, 1), [1d0,2d0,2d0])
    call add_test("node=1, (i, j) = (2,1)", factors%get_supernode_ptr(1, 2, 1), [3d0,3d0,4d0,4d0])
    call add_test("node=1, (i, j) = (3,1)", factors%get_supernode_ptr(1, 3, 1), [1d0,1d0,2d0,2d0])
    call add_test("node=1, (i, j) = (4,1)", factors%get_supernode_ptr(1, 4, 1), [3d0,3d0,4d0,4d0])
    call add_test("node=1, (i, j) = (5,1)", factors%get_supernode_ptr(1, 5, 1), [5d0,5d0])
    !call add_test("node=1, (i, j) = (2,2)", factors%get_supernode_ptr(1, 2, 2), [3d0,4d0,4d0])
    call add_test("node=1, (i, j) = (3,2)", factors%get_supernode_ptr(1, 3, 2), [1d0,1d0,2d0,2d0])
    call add_test("node=1, (i, j) = (4,2)", factors%get_supernode_ptr(1, 4, 2), [3d0,3d0,4d0,4d0])
    call add_test("node=1, (i, j) = (5,2)", factors%get_supernode_ptr(1, 5, 2), [5d0,5d0])
    !call add_test("node=2, (i, j) = (1,1)", factors%get_supernode_ptr(2, 1, 1), [1d0,2d0,2d0])
    call add_test("node=2, (i, j) = (2,1)", factors%get_supernode_ptr(2, 2, 1), [3d0,3d0,4d0,4d0])
    call add_test("node=2, (i, j) = (3,1)", factors%get_supernode_ptr(2, 3, 1), [5d0,5d0,1d0,1d0])
    call add_test("node=2, (i, j) = (4,1)", factors%get_supernode_ptr(2, 4, 1), [2d0,2d0,3d0,3d0])
    !call add_test("node=2, (i, j) = (2,2)", factors%get_supernode_ptr(2, 2, 2), [3d0,4d0,4d0])
    call add_test("node=2, (i, j) = (3,2)", factors%get_supernode_ptr(2, 3, 2), [5d0,5d0,1d0,1d0])
    call add_test("node=2, (i, j) = (4,2)", factors%get_supernode_ptr(2, 4, 2), [2d0,2d0,3d0,3d0])
    !call add_test("node=2, (i, j) = (3,3)", factors%get_supernode_ptr(2, 3, 3), [5d0,1d0])
    call add_test("node=2, (i, j) = (4,3)", factors%get_supernode_ptr(2, 4, 3), [2d0,3d0])
    
    !call add_test("node=3, (i, j) = (1,1)", factors%get_supernode_ptr(3, 1, 1), [1d0,2d0,2d0])
    call add_test("node=3, (i, j) = (2,1)", factors%get_supernode_ptr(3, 2, 1), [3d0,3d0,4d0,4d0])
    !call add_test("node=3, (i, j) = (2,2)", factors%get_supernode_ptr(3, 2, 2), [3d0,4d0,4d0])
    
    call end_array_tests()

    
  end subroutine
end program