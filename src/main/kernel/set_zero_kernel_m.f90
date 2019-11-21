module set_zero_kernel_m
  implicit none
  
contains

  subroutine set_zero_rect(a, ncol, nrow)
    double precision :: a(ncol, nrow)
    integer, intent(in) :: ncol, nrow

    a = 0d0

  end subroutine

  subroutine set_zero_tri(a, n)
    double precision :: a(n, n)
    integer, intent(in) :: n
    integer :: i, j

    do j=1, n
      do i=1, j
        a(i, j) = 0d0
      enddo
    enddo

  end subroutine

end module