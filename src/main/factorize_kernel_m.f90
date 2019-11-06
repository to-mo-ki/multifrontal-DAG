module factorize_kernel_m
  implicit none
contains
  subroutine mydpotrf(n, a, info)
    integer, intent(in) :: n
    double precision :: a(:)
    integer, intent(out) :: info
    call DPOTRF("U", n, a, n, info)
  end subroutine

  subroutine mydtrsm(m, n, a, b)
    integer, intent(in) :: m, n
    double precision, intent(in) :: a(:)
    double precision :: b(:)
    call DTRSM("L", "U", "T", "N", m, n, 1.0d0, a, m, b, m)
  end subroutine

  subroutine mydsyrk(n, k, a, c)
    integer, intent(in) :: n, k
    double precision, intent(in) :: a(:)
    double precision :: c(:)
    call dsyrk("U", "T", n, k, -1.0d0, a, k, 1.0d0, c, n)
  end subroutine

  subroutine mydgemm(m, n, a, b, c)
    ! C = C - B*A^T
    ! C^T = C^T-A^T*B
    ! A:n*n, B:n*m, C:n*m 
    integer, intent(in) :: m, n
    double precision, intent(in) :: a(:), b(:)
    double precision :: c(:)
    call dgemm("T", "N", m, n, m, -1.0d0, a, m, b, m, 1.0d0, c, m)
  end subroutine

end module