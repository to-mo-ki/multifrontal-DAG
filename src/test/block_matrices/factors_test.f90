program factors_test
  use contiguous_sets_m
  use jagged_array_m
  use factors_m
  use test_util
  use check_matrix_ptr_m
  implicit none

  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: ccs
  integer :: nb, ssize, wsize

  nb = 3
  node_sets => create_contiguous_sets((/1, 6, 4/))
  ccs => create_jagged_array((/4, 5, 0/))
  factors => create_factors(node_sets, ccs, nb)

  call assert_equal("num_node", factors%get_num_node(), 3)
  call check_num_blocks((/2, 4, 2/))

  call check_work_start_indices((/1, 3/))
  call check_exist_border((/.true., .false., .true./))
  call check_matrix_size(1, (/9, 6, 4/))
  call check_matrix_size(2, (/9, 9, 9, 6, 9, 9, 6, 9, 6, 4/))
  call check_matrix_size(3, (/9, 3, 1/))
  call check_block_size(1, (/3, 2/))
  call check_block_size(2, (/3, 3, 3, 2/))
  call check_block_size(3, (/3, 1/))
  call check_matrix_ptr(factors)
  
  call factors%get_border_info(1, ssize, wsize)
  call assert_equal("border_info:1", (/ssize, wsize/), (/1, 2/))
  
  nb = 3
  node_sets => create_contiguous_sets((/5, 6, 7, 5, 3, 6/))
  ccs => create_jagged_array((/5, 4, 4, 4, 6, 0/))
  factors => create_factors(node_sets, ccs, nb)

  call assert_equal("num_node", factors%get_num_node(), 6)
  call check_num_blocks((/4, 4, 4, 3, 3, 2/))
  call check_work_start_indices((/2, 3, 3, 2, 2/))
  call check_exist_border((/.true., .false., .true., .true., .false., .false./))
  call check_matrix_size(1, (/9, 9, 9, 3, 9, 9, 3, 9, 3, 1/))
  call check_matrix_size(2, (/9, 9, 9, 3, 9, 9, 3, 9, 3, 1/))
  call check_matrix_size(3, (/9, 9, 9, 6, 9, 9, 6, 9, 6, 4/))
  call check_matrix_size(4, (/9, 9, 9, 9, 9, 9/))
  call check_matrix_size(5, (/9, 9, 9, 9, 9, 9/))
  call check_matrix_size(6, (/9, 9, 9/))
  call check_block_size(1, (/3, 3, 3, 1/))
  call check_block_size(2, (/3, 3, 3, 1/))
  call check_block_size(3, (/3, 3, 3, 2/))
  call check_block_size(4, (/3, 3, 3/))
  call check_block_size(5, (/3, 3, 3/))
  call check_block_size(6, (/3, 3/))
  call check_matrix_ptr(factors)

  call factors%get_border_info(1, ssize, wsize)
  call assert_equal("border_info:1", (/ssize, wsize/), (/2, 1/))
  call factors%get_border_info(3, ssize, wsize)
  call assert_equal("border_info:3", (/ssize, wsize/), (/1, 2/))
  call factors%get_border_info(4, ssize, wsize)
  call assert_equal("border_info:4", (/ssize, wsize/), (/2, 1/))

contains
  subroutine check_num_blocks(check)
    integer, intent(in) :: check(*)
    integer :: i
    do i=1, factors%get_num_node()
      call assert_equal("num_block:node="//to_str(i), factors%get_num_block(i), check(i))
    enddo
  end subroutine
  
  subroutine check_work_start_indices(check)
    integer, intent(in) :: check(*)
    integer :: i
    do i=1, factors%get_num_node()-1
      call assert_equal("work_start_index:node="//to_str(i), factors%get_work_start_index(i), check(i))
    enddo
  end subroutine
  
  subroutine check_exist_border(check)
    logical, intent(in) :: check(*)
    integer :: i
    do i=1, factors%get_num_node()
      call assert_equal("exist_border:node="//to_str(i), factors%exist_border(i), check(i))
    enddo
  end subroutine
  
  subroutine check_matrix_size(node, check)
    integer, intent(in) :: node, check(*)
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, ptr
    
    ptr = 1
    do j=1, factors%get_num_block(node)
      do i=j, factors%get_num_block(node)
        matrix => factors%get_matrix_ptr(node, i, j)
        call assert_equal("matrix size:node="//to_str(node)//", i="//to_str(i)//", j="//to_str(j), size(matrix), check(ptr))
        ptr = ptr + 1
      enddo
    enddo
  
  end subroutine

  subroutine check_block_size(node, check)
    integer, intent(in) :: node, check(:)
    integer :: i
    
    do i=1, size(check)
      call assert_equal("get_block_size:"//to_str(i)//", "//to_str(node), factors%get_block_size(i, node), check(i))
    enddo
  end subroutine

end program factors_test

