module starpu_border_kernel_m
  use iso_c_binding
  use starpu_wrapper_m
  use border_kernel_m, seq_border_potrf => border_potrf
  use border_kernel_m, seq_border_trsm => border_trsm
  implicit none
  private
  public :: border_potrf, border_trsm

contains

  recursive subroutine border_potrf(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: work(:), supernode(:)
    integer :: supernode_size, work_size

    call get_arg(cl_args, supernode_size, work_size)
    call get_vector_ptr(buffers, 0, supernode)
    call get_vector_ptr(buffers, 1, work)
    
    call seq_border_potrf(supernode, work, supernode_size, work_size)
    
  end subroutine

  recursive subroutine border_trsm(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: diag_supernode(:), solve_work(:), solve_supernode(:)
    integer :: supernode_size, work_size, nrow

    call get_arg(cl_args, supernode_size, work_size, nrow)
    call get_vector_ptr(buffers, 0, diag_supernode)
    call get_vector_ptr(buffers, 1, solve_supernode)
    call get_vector_ptr(buffers, 2, solve_work)

    call seq_border_trsm(diag_supernode, solve_supernode, solve_work, supernode_size, work_size, nrow)

  end subroutine

end module