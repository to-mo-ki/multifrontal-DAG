module starpu_factorize_kernel_m
  use iso_c_binding
  use starpu_wrapper_m
  use factorize_kernel_m, seq_mydpotrf => mydpotrf
  use factorize_kernel_m, seq_mydtrsm => mydtrsm
  use factorize_kernel_m, seq_mydsyrk => mydsyrk
  use factorize_kernel_m, seq_mydgemm => mydgemm
  implicit none
  private
  public :: mydpotrf, mydtrsm, mydsyrk, mydgemm
  
contains
  recursive subroutine mydpotrf(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: a(:)
    integer :: ncol, info

    call get_arg1(cl_args, ncol)
    call get_vector_ptr(buffers, 0, a)
    call seq_mydpotrf(ncol, a, info)

  end subroutine

  recursive subroutine mydtrsm(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: a(:), b(:)
    integer :: ncol, nrow

    call get_arg2(cl_args, ncol, nrow)
    call get_vector_ptr(buffers, 0, a)
    call get_vector_ptr(buffers, 1, b)
    call seq_mydtrsm(ncol, nrow, a, b)

  end subroutine

  recursive subroutine mydgemm(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: upper_matrix(:), lower_matrix(:), update_matrix(:)
    integer :: ncol, nrow_upper, nrow_lower

    call get_arg3(cl_args, ncol, nrow_lower, nrow_upper)
    call get_vector_ptr(buffers, 0, lower_matrix)
    call get_vector_ptr(buffers, 1, upper_matrix)
    call get_vector_ptr(buffers, 2, update_matrix)
    call seq_mydgemm(ncol, nrow_lower, nrow_upper, lower_matrix, upper_matrix, update_matrix)

  end subroutine

  recursive subroutine mydsyrk(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: a(:), b(:)
    integer :: nrow, ncol

    call get_arg2(cl_args, nrow, ncol)
    call get_vector_ptr(buffers, 0, a)
    call get_vector_ptr(buffers, 1, b)
    call seq_mydsyrk(nrow, ncol, a, b)

  end subroutine

end module