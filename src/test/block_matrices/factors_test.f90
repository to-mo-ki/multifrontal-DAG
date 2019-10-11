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
  integer :: nb

  nb = 3
  node_sets => create_contiguous_sets((/1, 6, 4/))
  ccs => create_jagged_array((/4, 5, 0/))
  factors => create_factors(node_sets, ccs, nb)

  call check_num_blocks(3, (/2, 4, 2/))

  call check_work_start_indices(3, (/1, 3/))
  call check_exist_border(3, (/.true., .false., .true./))
  call check_matrix_size(1, (/9, 6, 4/))
  call check_matrix_size(2, (/9, 9, 9, 6, 9, 9, 6, 9, 6, 4/))
  call check_matrix_size(3, (/9, 3, 1/))
  call check_matrix_ptr(3, factors)
  
  nb = 3
  node_sets => create_contiguous_sets((/5, 6, 7, 5, 3, 6/))
  ccs => create_jagged_array((/5, 4, 4, 4, 6, 0/))
  factors => create_factors(node_sets, ccs, nb)

  call check_num_blocks(6, (/4, 4, 4, 3, 3, 2/))
  call check_work_start_indices(6, (/2, 3, 3, 2, 2/))
  call check_exist_border(6, (/.true., .false., .true., .true., .false., .false./))
  call check_matrix_size(1, (/9, 9, 9, 3, 9, 9, 3, 9, 3, 1/))
  call check_matrix_size(2, (/9, 9, 9, 3, 9, 9, 3, 9, 3, 1/))
  call check_matrix_size(3, (/9, 9, 9, 6, 9, 9, 6, 9, 6, 4/))
  call check_matrix_size(4, (/9, 9, 9, 9, 9, 9/))
  call check_matrix_size(5, (/9, 9, 9, 9, 9, 9/))
  call check_matrix_size(6, (/9, 9, 9/))
  call check_matrix_ptr(6, factors)

contains

  subroutine check_num_blocks(num_node, check)
    integer, intent(in) :: num_node, check(*)
    integer :: i
    do i=1, num_node
      call assert_equal("num_block:node="//trim(to_str(i)), factors%get_num_block(i), check(i))
    enddo
  end subroutine

  subroutine check_work_start_indices(num_node, check)
    integer, intent(in) :: num_node, check(*)
    integer :: i
    do i=1, num_node-1
      call assert_equal("work_start_index:node="//trim(to_str(i)), factors%get_work_start_index(i), check(i))
    enddo
  end subroutine

  subroutine check_exist_border(num_node, check)
    integer, intent(in) :: num_node
    logical, intent(in) :: check(*)
    integer :: i
    do i=1, num_node
      call assert_equal("exist_border:node="//trim(to_str(i)), factors%exist_border(i), check(i))
    enddo
  end subroutine

  subroutine check_matrix_size(node, check)
    use partial_sum_m
    integer, intent(in) :: node, check(*)
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, ptr
    
    ptr = 1
    do j=1, factors%get_num_block(node)
      do i=j, factors%get_num_block(node)
        matrix => factors%get_matrix_ptr(node, i, j)
        call assert_equal("matrix size:node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), size(matrix), check(ptr))
        ptr = ptr + 1
      enddo
    enddo

  end subroutine

end program factors_test