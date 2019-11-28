program block_matrices_test
  use jagged_array_m
  use contiguous_sets_m
  use block_matrices_m
  use matrix_controller_m
  use supernode_controller_m
  use work_controller_m
  use border_controller_m
  use test_util
  implicit none

  type(jagged_array_c), pointer :: ccs
  type(contiguous_sets_c), pointer :: node_sets
  type(block_matrices_c), pointer :: block_matrices
  class(matrix_controller_c), pointer :: controller
  character(len=10) :: message
  integer :: nb
  
  nb = 3
  node_sets => create_contiguous_sets([1, 6, 4])
  ccs => create_jagged_array([4, 5, 0])
  call check_supernode(1, 2, 1, [3, 2])
  call check_supernode(2, 4, 2, [9, 9, 9, 6, 9, 9, 6])
  call check_supernode(3, 2, 2, [9, 3, 1])
  call check_work(1, 1, 2, [4, 4, 4])
  call check_work(2, 3, 4, [9, 6, 4])
  call check_border(1, 1, 2, [9, 6])

  nb = 3
  node_sets => create_contiguous_sets([5, 6, 7, 5, 3, 6])
  ccs => create_jagged_array([5, 4, 4, 4, 6, 0])

  call check_supernode(1, 4, 2, [9, 9, 9, 3, 6, 6, 2])
  call check_supernode(2, 4, 2, [9, 9, 9, 3, 9, 9, 3])
  call check_supernode(3, 4, 3, [9, 9, 9, 6, 9, 9, 6, 3, 2])
  call check_supernode(4, 3, 2, [9, 9, 9, 6, 6])
  call check_supernode(5, 3, 1, [9, 9, 9])
  call check_supernode(6, 2, 2, [9, 9, 9])
  call check_work(1, 2, 4, [1, 3, 1, 9, 3, 1])
  call check_work(2, 3, 4, [9, 3, 1])
  call check_work(3, 3, 4, [4, 4, 4])
  call check_work(4, 2, 3, [1, 3, 9])
  call check_work(5, 2, 3, [9, 9, 9])
  call check_border(1, 2, 4, [9, 9, 3])
  call check_border(3, 3, 4, [9, 6])
  call check_border(4, 2, 3, [9, 9])
  
contains
  subroutine check_supernode(node, num_row, num_col, ans_size)
    integer, intent(in) :: node, num_row, num_col, ans_size(*)
    
    message = "supernode"
    allocate(supernode_controller_c::controller)
    block_matrices => create_block_matrices(nb, node_sets, ccs, controller)
    call check_matrix_size(node, 1, num_col, num_row, ans_size)
    call reset_matrix(node, 1, num_col, num_row)
    call prepare_matrix_ptr(node, 1, num_col, num_row)
    call check_matrix_ptr(node, 1, num_col, num_row)

  end subroutine

  subroutine check_work(node, jstart, num_row, ans_size)
    integer, intent(in) :: node, jstart, num_row, ans_size(*)

    message = "work"
    allocate(work_controller_c::controller)
    block_matrices => create_block_matrices(nb, node_sets, ccs, controller)
    call check_matrix_size(node, jstart, num_row, num_row, ans_size)
    call reset_matrix(node, jstart, num_row, num_row)
    call prepare_matrix_ptr(node, jstart, num_row, num_row)
    call check_matrix_ptr(node, jstart, num_row, num_row)

  end subroutine

  subroutine check_border(node, j, num_row, ans_size)
    integer, intent(in) :: node, j, num_row, ans_size(*)

    message = "border"
    allocate(border_controller_c::controller)
    block_matrices => create_block_matrices(nb, node_sets, ccs, controller)
    call check_matrix_size(node, j, j, num_row, ans_size)
    call reset_matrix(node, j, j, num_row)
    call prepare_matrix_ptr(node, j, j, num_row)
    call check_matrix_ptr(node, j, j, num_row)

  end subroutine

  subroutine reset_matrix(node, jstart, jend, num_row)
    integer, intent(in) :: node, jstart, jend, num_row
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j

    do j=jstart, jend
      do i=j, num_row
        matrix => block_matrices%get_ptr(node, i, j)
        matrix = 0.0d0
      enddo
    enddo

  end subroutine

  subroutine check_matrix_size(node, jstart, jend, num_row, ans_sizes)
    integer, intent(in) :: node, jstart, jend, num_row, ans_sizes(*)
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, ptr
    
    ptr = 1
    do j=jstart, jend
      do i=j, num_row
        matrix => block_matrices%get_ptr(node, i, j)
        call assert_equal(message//" size:node="//to_str(node)//", i="//to_str(i)//", j="//to_str(j), size(matrix), ans_sizes(ptr))
        ptr = ptr + 1
      enddo
    enddo

  end subroutine

  subroutine prepare_matrix_ptr(node, jstart, jend, num_row)
    integer, intent(in) :: node, jstart, jend, num_row
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, k, ptr
    
    ptr = 1
    do j=jstart, jend
      do i=j, num_row
        matrix => block_matrices%get_ptr(node, i, j)
        do k=1, size(matrix)
          matrix(k) = matrix(k) + dble(ptr)
          ptr = ptr + 1
        enddo
      enddo
    enddo

  end subroutine

  subroutine check_matrix_ptr(node, jstart, jend, num_row)
    integer, intent(in) :: node, jstart, jend, num_row
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, k, ptr

    ptr = 1
    do j=jstart, jend
      do i=j, num_row
        matrix => block_matrices%get_ptr(node, i, j)
        do k=1, size(matrix)
          if(nint(matrix(k)) /= ptr)then
            call fail_access_check(i, j, k, nint(matrix(k)), ptr)
            return
          endif
          ptr = ptr + 1
        enddo
      enddo
    enddo

    call success_message(message//" double access:node="//to_str(node))

  end subroutine

  subroutine fail_access_check(i, j, k, ans, check)
    integer, intent(in) :: i, j, k, ans, check
    call fail(message//" double access. "//&
      &"i="//to_str(i)//&
      &", j="//to_str(j)//&
      &", k="//to_str(k)//&
      &", ans="//to_str(ans)//&
      &", check="//to_str(check))

  end subroutine

end program block_matrices_test