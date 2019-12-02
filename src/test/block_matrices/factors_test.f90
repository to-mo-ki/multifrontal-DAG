program factors_test
  use contiguous_sets_m
  use jagged_array_m
  use factors_m
  use test_util
  use check_matrix_ptr_m
  use node_data_m
  implicit none

  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: ccs
  type(node_data_c), pointer :: node_data
  integer :: nb, ssize, wsize, i
  integer, allocatable :: ans(:)

  nb = 3
  node_sets => create_contiguous_sets([1, 6, 4])
  ccs => create_jagged_array([4, 5, 0])
  node_data => create_node_data([1,6,4], [4,5,0], nb)
  factors => create_factors(node_data, node_sets, ccs, nb)

  call assert_equal("num_node", factors%get_num_node(), 3)
  call check_num_blocks([2, 4, 2])

  call check_work_start_indices([1, 3])
  call check_exist_border([.true., .false., .true.])
  call check_matrix_size(1, [9, 6, 4])
  call check_matrix_size(2, [9, 9, 9, 6, 9, 9, 6, 9, 6, 4])
  call check_matrix_size(3, [9, 3, 1])
  call check_block_size(1, [3, 2])
  call check_block_size(2, [3, 3, 3, 2])
  call check_block_size(3, [3, 1])
  call check_matrix_ptr(factors)
  
  call factors%get_border_info(1, ssize, wsize)
  call assert_equal("border_info:1", [ssize, wsize], [1, 2])

  call start_tests("get_first")
  allocate(ans(3), source=[1,2,8])
  do i=1, 3
    call add_test(i, factors%get_first(i), ans(i))
  enddo
  call end_tests()
  deallocate(ans)

  call start_tests("get_last")
  allocate(ans(3), source=[1,7,11])
  do i=1, 3
    call add_test(i, factors%get_last(i), ans(i))
  enddo
  call end_tests()
  deallocate(ans)
  
  nb = 3
  node_sets => create_contiguous_sets([5, 6, 7, 5, 3, 6])
  ccs => create_jagged_array([5, 4, 4, 4, 6, 0])
  node_data => create_node_data([5, 6, 7, 5, 3, 6], [5, 4, 4, 4, 6, 0], nb)
  factors => create_factors(node_data, node_sets, ccs, nb)

  call assert_equal("num_node", factors%get_num_node(), 6)
  call check_num_blocks([4, 4, 4, 3, 3, 2])
  call check_work_start_indices([2, 3, 3, 2, 2])
  call check_exist_border([.true., .false., .true., .true., .false., .false.])
  call check_matrix_size(1, [9, 9, 9, 3, 9, 9, 3, 9, 3, 1])
  call check_matrix_size(2, [9, 9, 9, 3, 9, 9, 3, 9, 3, 1])
  call check_matrix_size(3, [9, 9, 9, 6, 9, 9, 6, 9, 6, 4])
  call check_matrix_size(4, [9, 9, 9, 9, 9, 9])
  call check_matrix_size(5, [9, 9, 9, 9, 9, 9])
  call check_matrix_size(6, [9, 9, 9])
  call check_block_size(1, [3, 3, 3, 1])
  call check_block_size(2, [3, 3, 3, 1])
  call check_block_size(3, [3, 3, 3, 2])
  call check_block_size(4, [3, 3, 3])
  call check_block_size(5, [3, 3, 3])
  call check_block_size(6, [3, 3])
  call check_matrix_ptr(factors)

  call factors%get_border_info(1, ssize, wsize)
  call assert_equal("border_info:1", [ssize, wsize], [2, 1])
  call factors%get_border_info(3, ssize, wsize)
  call assert_equal("border_info:3", [ssize, wsize], [1, 2])
  call factors%get_border_info(4, ssize, wsize)
  call assert_equal("border_info:4", [ssize, wsize], [2, 1])

  call start_tests("get_first")
  allocate(ans(6), source=[1,6,12,19,24,27])
  do i=1, 6
    call add_test(i, factors%get_first(i), ans(i))
  enddo
  call end_tests()
  deallocate(ans)

  call start_tests("get_last")
  allocate(ans(6), source=[5,11,18,23,26,32])
  do i=1, 6
    call add_test(i, factors%get_last(i), ans(i))
  enddo
  call end_tests()
  deallocate(ans)
  

contains
  subroutine check_num_blocks(check)
    integer, intent(in) :: check(*)
    integer :: i

    call start_tests("num_block")
    do i=1, factors%get_num_node()
      call add_test("node="//to_str(i), factors%get_num_block(i), check(i))
    enddo
    call end_tests()

  end subroutine
  
  subroutine check_work_start_indices(check)
    integer, intent(in) :: check(*)
    integer :: i

    call start_tests("work_start_index")
    do i=1, factors%get_num_node()-1
      call add_test("node="//to_str(i), factors%get_work_start_index(i), check(i))
    enddo
    call end_tests()

  end subroutine
  
  subroutine check_exist_border(check)
    logical, intent(in) :: check(*)
    integer :: i
    
    call start_tests("exist_border")
    do i=1, factors%get_num_node()
      call add_test("node="//to_str(i), factors%exist_border(i), check(i))
    enddo
    call end_tests()

  end subroutine
  
  subroutine check_matrix_size(node, check)
    integer, intent(in) :: node, check(*)
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, ptr
    
    call start_tests("matrix size")
    ptr = 1
    do j=1, factors%get_num_block(node)
      do i=j, factors%get_num_block(node)
        matrix => factors%get_matrix_ptr(node, i, j)
        call add_test("node="//to_str(node)//", i="//to_str(i)//", j="//to_str(j), size(matrix), check(ptr))
        ptr = ptr + 1
      enddo
    enddo
    call end_tests()
  
  end subroutine

  subroutine check_block_size(node, check)
    integer, intent(in) :: node, check(:)
    integer :: i
    
    call start_tests("get_block_size")
    do i=1, size(check)
      call add_test(to_str(i)//", "//to_str(node), factors%get_block_size(i, node), check(i))
    enddo
    call end_tests()

  end subroutine

end program factors_test

