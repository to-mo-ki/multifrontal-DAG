program rearrange_task_test
  use starpu_wrapper_m
  use rearrange_tasks_m, task_init => init, task_finalize => finalize
  use test_util
  use iso_c_binding
  implicit none

  call starpu_init
  call task_init
  call diag_test
  call ndiag_test
  call task_finalize
  call starpu_finalize

contains
  subroutine diag_test
    use matrix_splitter_m
    double precision, pointer, contiguous :: origin(:), left(:), right(:)
    type(c_ptr) :: origin_dh, left_dh, right_dh
    integer :: i, j

    allocate(origin(7*7), left(7*3), right(4*4))

    do i=1, 7
      do j=1, i
        origin((i-1)*7+j) = dble(i*10+j)
      enddo
    enddo

    left = -1
    right = -1

    call register_vector_data(origin_dh, origin)
    call register_vector_data(left_dh, left)
    call register_vector_data(right_dh, right)

    call split_tri_matrix_task%insert_task((/3, 4/), (/origin_dh, left_dh, right_dh/))
    call task_wait_for_all

    call assert_equal("diag:left", left, [double precision::11,-1,-1,21,22,-1,31,32,33,41,42,43,51,52,53,61,62,63,71,72,73])
    call assert_equal("diag:right", right, [double precision::44,-1,-1,-1,54,55,-1,-1,64,65,66,-1,74,75,76,77])

  end subroutine

  subroutine ndiag_test
    double precision, pointer, contiguous :: origin(:), left(:), right(:)
    type(c_ptr) :: origin_dh, left_dh, right_dh
    integer :: i, j

    allocate(origin(4*5), left(4*2), right(4*3))
    do i=1, 4
      do j=1, 5
        origin((i-1)*5+j) = dble(i*10+j)
      enddo
    enddo

    left = -1
    right = -1
    
    call register_vector_data(origin_dh, origin)
    call register_vector_data(left_dh, left)
    call register_vector_data(right_dh, right)

    call split_rect_matrix_task%insert_task((/2, 3, 4/), (/origin_dh, left_dh, right_dh/))
    call task_wait_for_all
    call assert_equal("diag:left", left, [double precision::11,12,21,22,31,32,41,42d0])
    call assert_equal("diag:right", right, [double precision::13,14,15,23,24,25,33,34,35,43,44,45])
  end subroutine
  
end program