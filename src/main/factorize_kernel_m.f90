module factorize_kernel_m
  implicit none
contains
  subroutine mydpotrf(n, a, info)
    integer, intent(in) :: n
    double precision :: a(:)
    integer, intent(out) :: info
    call DPOTRF("U", n, a, n, info)
  end subroutine

  subroutine mydtrsm(ld, nrows, diag, rect)
    integer, intent(in) :: nrows, ld
    double precision, intent(in) :: diag(:)
    double precision :: rect(:)
    call DTRSM("L", "U", "T", "N", ld, nrows, 1.0d0, diag, ld, rect, ld)
  end subroutine

  subroutine mydsyrk(ld_diag, ld_rect, rect, diag)
    integer, intent(in) :: ld_diag, ld_rect
    double precision, intent(in) :: rect(:)
    double precision :: diag(:)
    call dsyrk("U", "T", ld_diag, ld_rect, -1.0d0, rect, ld_rect, 1.0d0, diag, ld_diag)
  end subroutine

  subroutine mydgemm(ld, upper_n, lower_n, upper, lower, update)
    ! C = C - B*A^T
    ! C^T = C^T-A^T*B
    ! A(ld, upper_n), B(ld, lower_n), C(upper_n, lower_n)
    integer, intent(in) :: ld, upper_n, lower_n
    double precision, intent(in) :: upper(:), lower(:)
    double precision :: update(:)
    call dgemm("T", "N", upper_n, lower_n, ld, -1.0d0, upper, ld, lower, ld, 1.0d0, update, upper_n)
  end subroutine

end module