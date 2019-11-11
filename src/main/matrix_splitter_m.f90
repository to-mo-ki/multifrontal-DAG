module matrix_splitter_m
  implicit none
  
contains
  subroutine split_tri_matrix(border, supernode, work, ssize, wsize, n)
    integer, intent(in) :: ssize, wsize, n
    double precision :: border(n, n), supernode(ssize, n), work(wsize, n)
    integer :: i, j

    do i=1, ssize
      do j=1, i
        supernode(j, i) = border(j, i)
      enddo
    enddo

    do i=ssize+1, n
      do j=1, ssize
        supernode(j, i) = border(j, i)
      enddo
    enddo

    do i=1, wsize
      do j=1, wsize
        work(j, i) = border(j+ssize, i+ssize)
      enddo
    enddo

  end subroutine

  subroutine split_rect_matrix(border, supernode, work, ssize, wsize, nrow, ncol)
    integer, intent(in) :: ssize, wsize, nrow, ncol
    double precision :: border(ncol, nrow), supernode(ssize, nrow), work(wsize, nrow)
    integer :: i, j

    do i=1, nrow
      do j=1, ssize
        supernode(j, i) = border(j, i)
      enddo
    enddo

    do i=1, nrow
      do j=1, wsize
        work(j, i) = border(ssize+j, i)
      enddo
    enddo
  end subroutine

end module