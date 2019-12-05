program extend_add_subroutines_test
  use extend_add_subroutines_m
  use factors_m
  use contiguous_sets_m
  use jagged_array_m
  use node_data_m
  use block_local_index_m
  use zero_setter_m
  use test_util
  implicit none
  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_set
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(block_local_index_c), pointer :: block_local_index
  double precision, pointer, contiguous :: a(:)

  call test2

contains
  subroutine test1()
    local_index => create_jagged_array([5, 3, 0], [2, 3, 4, 5, 7, 3, 4])
    node_set => create_contiguous_sets([4, 5, 4])
    node_data => create_node_data([5, 3, 0], [4, 5, 4],2)
    factors => create_factors(node_data, local_index, 2)
    
    call set_zero(factors)
    a => factors%get_work_ptr(1, 3, 3)
    a = [55d0,0d0,65d0,66d0]
    a => factors%get_work_ptr(1, 4, 3)
    a = [75d0,76d0,85d0,86d0]
    a => factors%get_work_ptr(1, 5, 3)
    a = [95d0,96d0]
    a => factors%get_work_ptr(1, 4, 4)
    a = [77d0,0d0,87d0,88d0]
    a => factors%get_work_ptr(1, 5, 4)
    a = [97d0,98d0]
    a => factors%get_work_ptr(1, 5, 5)
    a = [99d0]
    !call extend_add_diag(factors, 3, 1, 1, 2, [2], 0)
    a => factors%get_work_ptr(2, 1, 1)
    call assert_equal("(1,1)", a(4), 55d0)
    
    !call extend_add_ndiag(factors, 3, 3, 2, 1, 1, 2, [1], [2], 1, 0)
    !call extend_add_ndiag(factors, 4, 3, 2, 1, 1, 2, [2], [2], 0, 0)
    a => factors%get_work_ptr(2, 2, 1)
    call assert_equal("(2,1)", a, [0d0,65d0,0d0,75d0])
    
    !call extend_add_ndiag(factors, 4, 3, 3, 1, 1, 2, [1], [2], 1, 0)
    a => factors%get_work_ptr(2, 3, 1)
    call assert_equal("(3,1)", a, [0d0,85d0,0d0,0d0])
    
    !call extend_add_ndiag(factors, 5, 3, 4, 1, 1, 2, [1], [2], 0, 0)
    a => factors%get_work_ptr(2, 4, 1)
    call assert_equal("(4,1)", a, [0d0,95d0,0d0,0d0])
  end subroutine

  subroutine test2
    local_index => create_jagged_array([7, 3, 0], [3,4,5,6,8,9,10,1,2,3])
    node_set => create_contiguous_sets([4, 7, 3])
    node_data => create_node_data([4,7,3],[7,3,0],3)
    factors => create_factors(node_data, local_index, 3)
    node_data => create_node_data([4,7,3], [7,3,0], 3)
    block_local_index => create_block_local_index(node_data, local_index)
    
    call set_zero(factors)
    a => factors%get_work_ptr(1,2,2)
    a = [11,0,21,22]
    a => factors%get_work_ptr(1,3,2)
    a = [31,32,41,42,51,52]
    a => factors%get_work_ptr(1,4,2)
    a = [61,62,71,72]
    a => factors%get_work_ptr(1,3,3)
    a = [33,0,0,43,44,0,53,54,55]
    a => factors%get_work_ptr(1,4,3)
    a = [63,64,65,73,74,75]
    a => factors%get_work_ptr(1,4,4)
    a = [66,0,76,77]
    
    call start_tests("test2")
    call extend_add_diag(factors, block_local_index, 1, 1, 2)
    a => factors%get_matrix_ptr(2,1,1)
    call add_test("(1,1)=>(3,3)", a(9), 11d0)
    call extend_add_ndiag(factors, block_local_index, 2, 1, 1, 2)
    a => factors%get_matrix_ptr(2,2,1)
    call add_test("(2,1)=>(4,3)", a(3), 21d0)
    call extend_add_ndiag(factors, block_local_index, 3, 1, 1, 2)
    a => factors%get_matrix_ptr(2,2,1)
    call add_test("(3,1)=>(5,3)", a(6), 31d0)
    call add_test("(4,1)=>(6,3)", a(9), 41d0)
    call extend_add_ndiag(factors, block_local_index, 4, 1, 1, 2)
    a => factors%get_matrix_ptr(2,3,1)
    call add_test("(5,1)=>(8,3)", a(6), 51d0)
    call extend_add_ndiag(factors, block_local_index, 5, 1, 1, 2)
    a => factors%get_matrix_ptr(2,3,1)
    call add_test("(6,1)=>(9,3)", a(9), 61d0)
    call extend_add_ndiag(factors, block_local_index, 6, 1, 1, 2)
    a => factors%get_matrix_ptr(2,4,1)
    call add_test("(7,1)=>(10,3)", a(3), 71d0)
    call extend_add_diag(factors, block_local_index, 2, 1, 2)
    a => factors%get_matrix_ptr(2,2,2)
    call add_test("(2,2)=>(4,4)", a(1), 22d0)
    call extend_add_ndiag(factors, block_local_index, 3, 2, 1, 2)
    a => factors%get_matrix_ptr(2,2,2)
    call add_test("(3,2)=>(5,4)", a(4), 32d0)
    call add_test("(4,2)=>(6,4)", a(7), 42d0)
    call extend_add_ndiag(factors, block_local_index, 4, 2, 1, 2)
    a => factors%get_matrix_ptr(2,3,2)
    call add_test("(5,2)=>(8,4)", a(4), 52d0)
    call extend_add_ndiag(factors, block_local_index, 5, 2, 1, 2)
    a => factors%get_matrix_ptr(2,3,2)
    call add_test("(6,2)=>(9,4)", a(7), 62d0)
    call extend_add_ndiag(factors, block_local_index, 6, 2, 1, 2)
    a => factors%get_matrix_ptr(2,4,2)
    call add_test("(7,2)=>(10,4)", a(1), 72d0)
    call extend_add_diag(factors, block_local_index, 3, 1, 2)
    a => factors%get_matrix_ptr(2,2,2)
    call add_test("(3,3)=>(5,5)", a(5), 33d0)
    call add_test("(4,3)=>(6,5)", a(8), 43d0)
    call add_test("(4,4)=>(6,6)", a(9), 44d0)
    call extend_add_ndiag(factors, block_local_index, 4, 3, 1, 2)
    a => factors%get_matrix_ptr(2,3,2)
    call add_test("(5,3)=>(8,5)", a(5), 53d0)
    call add_test("(5,4)=>(8,6)", a(6), 54d0)
    call extend_add_ndiag(factors, block_local_index, 5, 3, 1, 2)
    a => factors%get_matrix_ptr(2,3,2)
    call add_test("(6,3)=>(9,5)", a(8), 63d0)
    call add_test("(6,4)=>(9,6)", a(9), 64d0)
    call extend_add_ndiag(factors, block_local_index, 6, 3, 1, 2)
    a => factors%get_matrix_ptr(2,4,2)
    call add_test("(7,3)=>(10,5)", a(2), 73d0)
    call add_test("(7,4)=>(10,6)", a(3), 74d0)
    call extend_add_diag(factors, block_local_index, 4, 1, 2)
    a => factors%get_matrix_ptr(2,3,3)
    call add_test("(5,5)=>(8,8)", a(5), 55d0)
    call extend_add_ndiag(factors, block_local_index, 5, 4, 1, 2)
    a => factors%get_matrix_ptr(2,3,3)
    call add_test("(6,5)=>(9,8)", a(8), 65d0)
    call extend_add_ndiag(factors, block_local_index, 6, 4, 1, 2)
    a => factors%get_matrix_ptr(2,4,3)
    call add_test("(7,5)=>(10,8)", a(2), 75d0)
    call extend_add_diag(factors, block_local_index, 5, 1, 2)
    a => factors%get_matrix_ptr(2,3,3)
    call add_test("(6,6)=>(9,9)", a(9), 66d0)
    call extend_add_ndiag(factors, block_local_index, 6, 5, 1, 2)
    a => factors%get_matrix_ptr(2,4,3)
    call add_test("(7,6)=>(10,9)", a(3), 76d0)
    call extend_add_diag(factors, block_local_index, 6, 1, 2)
    a => factors%get_matrix_ptr(2,4,4)
    call add_test("(7,7)=>(10,10)", a(1), 77d0)
    call end_tests()
    
  end subroutine
  
end program extend_add_subroutines_test