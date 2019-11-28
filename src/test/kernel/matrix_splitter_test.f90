program matrix_splitter_test
  use matrix_splitter_m
  use test_util
  implicit none
  call diag_test()
  call rect_test()

contains
  subroutine diag_test()
    double precision :: origin(7, 7), left(3*7), right(4*4)
    integer :: pos_left(18), pos_right(10)
    double precision :: check_left(18), check_right(10)
    integer :: i, j
    
    do i=1, 7
      do j=1, i
        origin(j, i) = dble(i*10+j)
      enddo
    enddo
    call split_tri_matrix(origin, left, right, 3, 4, 7)
    
    pos_left = [1, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]
    check_left = [11d0, 21d0, 22d0, 31d0, 32d0, 33d0, 41d0, 42d0, 43d0, 51d0, 52d0, 53d0, 61d0, 62d0, 63d0, 71d0, 72d0, 73d0]
    call assert_equal_partial_array("diag:left", left, pos_left, 18, check_left)
    
    pos_right = [1, 5, 6, 9, 10, 11, 13, 14, 15, 16]
    check_right = [44d0, 54d0, 55d0, 64d0, 65d0, 66d0, 74d0, 75d0, 76d0, 77d0]
    call assert_equal_partial_array("diag:right", right, pos_right, 10, check_right)

  end subroutine

  subroutine rect_test()
    double precision :: origin(5, 4), left(2*4), right(3*4)
    double precision :: check_left(8), check_right(12)
    integer :: i, j
    
    do i=1, 4
      do j=1, 5
        origin(j, i) = dble(i*10+j)
      enddo
    enddo

    call split_rect_matrix(origin, left, right, 2, 3, 4, 5)
    check_left = [11d0, 12d0, 21d0, 22d0, 31d0, 32d0, 41d0, 42d0]
    call assert_equal("rect:left", left, check_left)
    check_right = [13d0, 14d0, 15d0, 23d0, 24d0, 25d0, 33d0, 34d0, 35d0, 43d0, 44d0, 45d0]
    call assert_equal("rect:right", right, check_right)

  end subroutine

  
end program matrix_splitter_test