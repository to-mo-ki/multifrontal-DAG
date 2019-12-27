program matrix_splitter_test
  use matrix_splitter_m
  use test_util
  implicit none
  call diag_test()
  call rect_test()

contains
  subroutine diag_test()
    double precision :: origin(7*7), left(3*7), right(4*4)
    integer :: i, j
    
    do i=1, 7
      do j=1, i
        origin((i-1)*7+j) = dble(i*10+j)
      enddo
    enddo

    left = -1
    right = -1

    call split_tri_matrix(origin, left, right, 3, 4)
    
    call assert_equal("diag:left", left, [11,-1,-1,21,22,-1,31,32,33,41,42,43,51,52,53,61,62,63,71,72,73])
    call assert_equal("diag:right", right, [44,-1,-1,-1,54,55,-1,-1,64,65,66,-1,74,75,76,77])

  end subroutine

  subroutine rect_test()
    double precision :: origin(5, 4), left(2*4), right(3*4)
    integer :: i, j
    
    do i=1, 4
      do j=1, 5
        origin(j, i) = dble(i*10+j)
      enddo
    enddo

    call split_rect_matrix(origin, left, right, 2, 3, 4)
    call assert_equal("rect:left", left, [11,12,21,22,31,32,41,42])
    call assert_equal("rect:right", right, [13,14,15,23,24,25,33,34,35,43,44,45])

  end subroutine

  
end program matrix_splitter_test