module border_solve_kernel_m
  use solve_kernel_m
  implicit none
  private

  public :: border_forward, border_backward
  
contains
  subroutine border_forward(matrix, rh, ncol, nrow)
    double precision, contiguous :: matrix(:), rh(:)
    integer, intent(in) :: ncol, nrow

    call mydtrsv_l(matrix, ncol, rh)
    call mydgemv_t(matrix(ncol*ncol+1:), ncol, nrow, rh, rh(ncol+1:))

  end subroutine

  subroutine border_backward(matrix, rh, ncol, nrow)
    double precision, contiguous :: matrix(:), rh(:)
    integer, intent(in) :: ncol, nrow

    call mydgemv_n(matrix(ncol*ncol+1:), ncol, nrow, rh(ncol+1:), rh)
    call mydtrsv_u(matrix, ncol, rh)

  end subroutine
end module