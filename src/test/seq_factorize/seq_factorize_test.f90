program seq_factorize_test
  use factors_m
  use block_local_index_info_m
  use jagged_array_m
  use jagged_array_3D_m
  use contiguous_sets_m
  use seq_factorize_m
  use test_util
  use coefficient_setter_m
  use ccs_m
  use node_data_m
  implicit none
  type(factors_c), pointer :: factors
  type(jagged_array_3D_c), pointer :: block_local_index
  type(block_local_index_info_c), pointer :: block_local_index_info
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: local_index, supernodal_index
  type(ccs_c), pointer :: ccs
  type(node_data_c), pointer :: node_data
  double precision, pointer, contiguous :: val(:), matrix(:)
  integer :: nb

  nb = 2
  node_sets => create_contiguous_sets([4,5,4])
  local_index => create_jagged_array([5,3,0],[2,3,4,5,7,1,3,4])
  allocate(val(70))
  val = [1,2,3,4,1,2,3,4,5,8,12,16,4,8,12,16,20,27,36,9,18,27,36,45,64,16,32,48,64,80,1,2,3,4,5,1,2,3,12,20,28,36,4,28,12,43,60,77,9,58,27,100,128,16,92,48,189,25,130,75,6,2,13,19,8,12,16,147,66,109]
  supernodal_index => create_jagged_array([9,8,7,6,8,7,6,5,4,4,3,2,1],[1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,3,4,5,6,7,8,9,4,5,6,7,8,9,1,2,3,4,5,6,7,8,2,3,4,5,6,7,8,3,4,5,6,7,8,4,5,6,7,8,5,6,7,8,1,2,3,4,2,3,4,3,4,4])
  ccs => create_ccs(supernodal_index, val)
  node_data => create_node_data([4,5,4],[5,3,0],nb)
  factors => create_factors(node_data)
  block_local_index_info => create_block_local_index_info(node_data, local_index)
  block_local_index => block_local_index_info%create_block_local_index()
  call set_coefficient(node_data, ccs, node_sets, factors)
  call seq_factorize(node_data, factors, block_local_index, block_local_index_info, [2,3,0])
  call check_nb_2()

  nb = 3
  node_sets => create_contiguous_sets([4,5,4])
  local_index => create_jagged_array([5,3,0],[2,3,4,5,7,1,3,4])
  allocate(val(70))
  val = [1,2,3,4,1,2,3,4,5,8,12,16,4,8,12,16,20,27,36,9,18,27,36,45,64,16,32,48,64,80,1,2,3,4,5,1,2,3,12,20,28,36,4,28,12,43,60,77,9,58,27,100,128,16,92,48,189,25,130,75,6,2,13,19,8,12,16,147,66,109]
  supernodal_index => create_jagged_array([9,8,7,6,8,7,6,5,4,4,3,2,1],[1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,3,4,5,6,7,8,9,4,5,6,7,8,9,1,2,3,4,5,6,7,8,2,3,4,5,6,7,8,3,4,5,6,7,8,4,5,6,7,8,5,6,7,8,1,2,3,4,2,3,4,3,4,4])
  ccs => create_ccs(supernodal_index, val)
  node_data => create_node_data([4,5,4],[5,3,0],nb)
  factors => create_factors(node_data)
  block_local_index_info => create_block_local_index_info(node_data, local_index)
  block_local_index => block_local_index_info%create_block_local_index()
  call set_coefficient(node_data, ccs, node_sets, factors)
  call seq_factorize(node_data, factors, block_local_index, block_local_index_info, [2,3,0])
  call check_nb_3()
  
contains

  subroutine check_nb_2
    call start_array_tests("nb=2")
    call add_test_tri("node=1, (i, j) = (1,1)", factors%get_supernode(1, 1, 1), [1d0,2d0,2d0], 2)
    call add_test("node=1, (i, j) = (2,1)", factors%get_supernode(1, 2, 1), [3d0,3d0,4d0,4d0])
    call add_test("node=1, (i, j) = (3,1)", factors%get_supernode(1, 3, 1), [1d0,1d0,2d0,2d0])
    call add_test("node=1, (i, j) = (4,1)", factors%get_supernode(1, 4, 1), [3d0,3d0,4d0,4d0])
    call add_test("node=1, (i, j) = (5,1)", factors%get_supernode(1, 5, 1), [5d0,5d0])
    call add_test_tri("node=1, (i, j) = (2,2)", factors%get_supernode(1, 2, 2), [3d0,4d0,4d0], 2)
    call add_test("node=1, (i, j) = (3,2)", factors%get_supernode(1, 3, 2), [1d0,1d0,2d0,2d0])
    call add_test("node=1, (i, j) = (4,2)", factors%get_supernode(1, 4, 2), [3d0,3d0,4d0,4d0])
    call add_test("node=1, (i, j) = (5,2)", factors%get_supernode(1, 5, 2), [5d0,5d0])
    call add_test_tri("node=2, (i, j) = (1,1)", factors%get_supernode(2, 1, 1), [1d0,2d0,2d0], 2)
    call add_test("node=2, (i, j) = (2,1)", factors%get_supernode(2, 2, 1), [3d0,3d0,4d0,4d0])
    call add_test("node=2, (i, j) = (3,1)", factors%get_supernode(2, 3, 1), [5d0,5d0,1d0,1d0])
    call add_test("node=2, (i, j) = (4,1)", factors%get_supernode(2, 4, 1), [2d0,2d0,3d0,3d0])
    call add_test_tri("node=2, (i, j) = (2,2)", factors%get_supernode(2, 2, 2), [3d0,4d0,4d0], 2)
    call add_test("node=2, (i, j) = (3,2)", factors%get_supernode(2, 3, 2), [5d0,5d0,1d0,1d0])
    call add_test("node=2, (i, j) = (4,2)", factors%get_supernode(2, 4, 2), [2d0,2d0,3d0,3d0])
    call add_test("node=2, (i, j) = (4,3)", factors%get_supernode(2, 4, 3), [2d0,3d0])
    call add_test_tri("node=3, (i, j) = (1,1)", factors%get_supernode(3, 1, 1), [1d0,2d0,2d0], 2)
    call add_test("node=3, (i, j) = (2,1)", factors%get_supernode(3, 2, 1), [3d0,3d0,4d0,4d0])
    call add_test_tri("node=3, (i, j) = (2,2)", factors%get_supernode(3, 2, 2), [3d0,4d0,4d0], 2)
    call end_array_tests()
    
  end subroutine

  subroutine check_nb_3
    call start_array_tests("nb=3")
    call add_test_tri("node=1, (i, j) = (1,1)", factors%get_supernode(1, 1, 1), [1d0,2d0,2d0,3d0,3d0,3d0], 3)
    call add_test("node=1, (i, j) = (2,1)", factors%get_supernode(1, 2, 1), [4d0,4d0,4d0,1d0,1d0,1d0,2d0,2d0,2d0])
    call add_test("node=1, (i, j) = (3,1)", factors%get_supernode(1, 3, 1), [3d0,3d0,3d0,4d0,4d0,4d0,5d0,5d0,5d0])
    call add_test("node=1, (i, j) = (2,2)", factors%get_supernode(1, 2, 2), [4d0,1d0,2d0])
    call add_test("node=1, (i, j) = (3,2)", factors%get_supernode(1, 3, 2), [3d0,4d0,5d0])
    call add_test_tri("node=2, (i, j) = (1,1)", factors%get_supernode(2, 1, 1), [1d0,2d0,2d0,3d0,3d0,3d0], 3)
    call add_test("node=2, (i, j) = (2,1)", factors%get_supernode(2, 2, 1), [4d0,4d0,4d0,5d0,5d0,5d0,1d0,1d0,1d0])
    call add_test("node=2, (i, j) = (3,1)", factors%get_supernode(2, 3, 1), [2d0,2d0,2d0,3d0,3d0,3d0])
    matrix => factors%get_supernode(2, 2, 2)
    call add_test_tri("node=2, (i, j) = (2,2):tri", matrix(1:4), [4d0,5d0,5d0], 2)
    call add_test("node=2, (i, j) = (2,2):rect", matrix(5:6), [1d0,1d0])
    call add_test("node=2, (i, j) = (3,2)", factors%get_supernode(2, 3, 2), [2d0,2d0,3d0,3d0])
    call add_test_tri("node=3, (i, j) = (1,1)", factors%get_supernode(3, 1, 1), [1d0,2d0,2d0,3d0,3d0,3d0], 3)
    call add_test("node=3, (i, j) = (2,1)", factors%get_supernode(3, 2, 1), [4d0,4d0,4d0])
    call add_test_tri("node=3, (i, j) = (2,2)", factors%get_supernode(3, 2, 2), [4d0], 1)
    call end_array_tests()
    
  end subroutine

  
end program