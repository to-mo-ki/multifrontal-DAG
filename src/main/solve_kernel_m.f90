module solve_kernel_m
  implicit none
contains
  subroutine mydtrsv_l(a, n, b)
    integer, intent(in) :: n
    double precision, intent(in) :: a(:)
    double precision :: b(:)
    call dtrsv("U", "T", "N", n, a, n, b, 1)
  end subroutine

  subroutine mydtrsv_u(a, n, b)
    integer, intent(in) :: n
    double precision, intent(in) :: a(:)
    double precision :: b(:)
    call dtrsv("U", "N", "N", n, a, n, b, 1)
  end subroutine

  subroutine mydgemv_t(a, m, n, x, y)
    integer, intent(in) :: m, n 
    double precision, intent(in) :: a(:), x(:)
    double precision :: y(:)
    call dgemv("T", m, n, -1d0, a, m, x, 1, 1d0, y, 1)
  end subroutine
  
  subroutine mydgemv_n(a, m, n, x, y)
    integer, intent(in) :: m, n 
    double precision, intent(in) :: a(:), x(:)
    double precision :: y(:)
    call dgemv("N", m, n, -1d0, a, m, x, 1, 1d0, y, 1)
  end subroutine

end module