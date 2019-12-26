module matrix_splitter_m
  implicit none
  
contains
  subroutine split_tri_matrix(origin, left, right, lsize, rsize)
    integer, intent(in) :: lsize, rsize
    double precision :: origin(lsize+rsize, *), left(lsize, *), right(rsize, *)
    integer :: i, j

    do i=1, lsize
      do j=1, i
        left(j, i) = origin(j, i)
      enddo
    enddo

    do i=lsize+1, lsize+rsize
      do j=1, lsize
        left(j, i) = origin(j, i)
      enddo
    enddo

    do i=1, rsize
      do j=1, i
        right(j, i) = origin(j+lsize, i+lsize)
      enddo
    enddo

  end subroutine

  subroutine split_rect_matrix(origin, left, right, lsize, rsize, nrow)
    integer, intent(in) :: lsize, rsize, nrow
    double precision :: origin(lsize+rsize, nrow), left(lsize, nrow), right(rsize, nrow)
    integer :: i, j

    do i=1, nrow
      do j=1, lsize
        left(j, i) = origin(j, i)
      enddo
    enddo

    do i=1, nrow
      do j=1, rsize
        right(j, i) = origin(lsize+j, i)
      enddo
    enddo
  end subroutine

end module