module border_solve_kernel_m
  use solve_kernel_m
  implicit none
  private

  public :: border_dtrsv_l, border_dtrsv_u
  
contains
  subroutine border_dtrsv_l(matrix, rh1, rh2, ncol, nrow)
    double precision, contiguous :: matrix(:), rh1(:), rh2(:)
    integer, intent(in) :: ncol, nrow

    call mydtrsv_l(matrix, ncol, rh1)
    call mydgemv_t(matrix(ncol*ncol+1:), ncol, nrow, rh1, rh2)

  end subroutine

  subroutine border_dtrsv_u(matrix, rh1, rh2, ncol, nrow)
    double precision, contiguous :: matrix(:), rh1(:), rh2(:)
    integer, intent(in) :: ncol, nrow

    call mydgemv_n(matrix(ncol*ncol+1:), ncol, nrow, rh2, rh1)
    call mydtrsv_u(matrix, ncol, rh1)

  end subroutine
end module