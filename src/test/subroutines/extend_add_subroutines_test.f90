program extend_add_subroutines_test
  use extend_add_subroutines_m
  use factors_m
  use contiguous_sets_m
  use jagged_array_m
  use zero_setter_m
  use test_util
  implicit none
  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_set
  type(jagged_array_c), pointer :: local_index
  double precision, pointer, contiguous :: a(:)

  call test2

contains
  subroutine test1()
    local_index => create_jagged_array((/5, 3, 0/), (/2, 3, 4, 5, 7, 3, 4/))
    node_set => create_contiguous_sets((/4, 5, 4/))
    factors => create_factors(node_set, local_index, 2)
    
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
    call extend_add_diag(factors, 3, 1, 1, 2, [2], 0)
    a => factors%get_matrix_ptr(2, 1, 1)
    call assert_equal("(1,1)", a(4), 55d0)
    
    call extend_add_ndiag(factors, 3, 3, 2, 1, 1, 2, [1], [2], 1, 0)
    call extend_add_ndiag(factors, 4, 3, 2, 1, 1, 2, [2], [2], 0, 0)
    a => factors%get_matrix_ptr(2, 2, 1)
    call assert_equal("(2,1)", a, [0d0,65d0,0d0,75d0])
    
    call extend_add_ndiag(factors, 4, 3, 3, 1, 1, 2, [1], [2], 1, 0)
    a => factors%get_matrix_ptr(2, 3, 1)
    call assert_equal("(3,1)", a, [0d0,85d0,0d0,0d0])
    
    call extend_add_ndiag(factors, 5, 3, 4, 1, 1, 2, [1], [2], 0, 0)
    a => factors%get_matrix_ptr(2, 4, 1)
    call assert_equal("(4,1)", a, [0d0,95d0,0d0,0d0])
  end subroutine

  subroutine test2
    local_index => create_jagged_array((/7, 3, 0/), (/3,4,5,6,8,9,10,1,2,3/))
    node_set => create_contiguous_sets((/4, 7, 3/))
    factors => create_factors(node_set, local_index, 3)
    
    call set_zero(factors)
    a => factors%get_matrix_ptr(1,2,2)
    a = [0,0,0,0,11,0,0,21,22]
    a => factors%get_matrix_ptr(1,3,2)
    a = [0,31,32,0,41,42,0,51,52]
    a => factors%get_matrix_ptr(1,4,2)
    a = [0,61,62,0,71,72]
    a => factors%get_matrix_ptr(1,3,3)
    a = [33,0,0,43,44,0,53,54,55]
    a => factors%get_matrix_ptr(1,4,3)
    a = [63,64,65,73,74,75]
    a => factors%get_matrix_ptr(1,4,4)
    a = [66,0,76,77]

    call extend_add_diag(factors, 2, 1, 1, 2, [3], 1)
    a => factors%get_matrix_ptr(2,1,1)
    call assert_equal("(1,1)", a(9), 11d0)
    call extend_add_ndiag(factors, 2, 1, 2,1,1,2,[4,5,6],[3],1,0)
    call extend_add_ndiag(factors,2,1,2,1,1,2,[5,6],[3],0,0)
    
  end subroutine
  
end program extend_add_subroutines_test