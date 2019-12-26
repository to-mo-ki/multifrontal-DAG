program matrix_splitter_test
  use matrix_splitter_m
  use test_util
  implicit none
  call diag_test()
  call rect_test()

contains
  subroutine diag_test()
    double precision :: origin(7*7), left(3*7), right(4*4)
    integer :: pos_left(18), pos_right(10)
    double precision :: check_left(18), check_right(10)
    integer :: i, j
    
    do i=1, 7
      do j=1, i
        origin((i-1)*7+j) = dble(i*10+j)
      enddo
    enddo

    left = -1
    right = -1

    call split_tri_matrix(origin, left, right, 3, 4)
    
    call assert_equal("diag:left", left, [double precision::11,-1,-1,21,22,-1,31,32,33,41,42,43,51,52,53,61,62,63,71,72,73])
    call assert_equal("diag:right", right, [double precision::44,-1,-1,-1,54,55,-1,-1,64,65,66,-1,74,75,76,77])

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

    call split_rect_matrix(origin, left, right, 2, 3, 4)
    check_left = [11d0, 12d0, 21d0, 22d0, 31d0, 32d0, 41d0, 42d0]
    call assert_equal("rect:left", left, check_left)
    check_right = [13d0, 14d0, 15d0, 23d0, 24d0, 25d0, 33d0, 34d0, 35d0, 43d0, 44d0, 45d0]
    call assert_equal("rect:right", right, check_right)

  end subroutine

  
end program matrix_splitter_test