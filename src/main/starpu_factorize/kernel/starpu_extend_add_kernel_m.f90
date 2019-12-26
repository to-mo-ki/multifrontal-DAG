module starpu_extend_add_kernel_m
  use iso_c_binding
  use starpu_wrapper_m
  use extend_add_kernel_m, seq_extend_add_rect => extend_add_rect
  use extend_add_kernel_m, seq_extend_add_tri => extend_add_tri
  implicit none
  private
  public :: extend_add_rect, extend_add_tri
  
contains

  recursive subroutine extend_add_rect(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: from(:), to(:)
    integer, pointer, contiguous  :: col_local(:), row_local(:)
    integer :: ld_from, ld_to, coffset, roffset
    
    call get_arg(cl_args, ld_from, ld_to, coffset, roffset)
    call get_vector_ptr(buffers, 0, from)
    call get_vector_ptr(buffers, 1, to)
    call get_vector_ptr(buffers, 2, col_local)
    call get_vector_ptr(buffers, 3, row_local)

    call seq_extend_add_rect(from, to, col_local, row_local, ld_from, ld_to, coffset, roffset)

  end subroutine

  subroutine extend_add_tri(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: from(:), to(:)
    integer, pointer, contiguous  :: local(:)
    integer :: ld_from, ld_to, offset

    call get_arg(cl_args, ld_from, ld_to, offset)
    call get_vector_ptr(buffers, 0, from)
    call get_vector_ptr(buffers, 1, to)
    call get_vector_ptr(buffers, 2, local)

    call seq_extend_add_tri(from, to, local, ld_from, ld_to, offset)

  end subroutine

end module