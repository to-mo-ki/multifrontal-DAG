module starpu_matrix_splitter_m
  use iso_c_binding
  use starpu_wrapper_m
  use matrix_splitter_m, seq_split_tri_matrix => split_tri_matrix
  use matrix_splitter_m, seq_split_rect_matrix => split_rect_matrix
  implicit none
  private
  public :: split_tri_matrix, split_rect_matrix
  
contains
  recursive subroutine split_tri_matrix(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: origin(:), left(:), right(:)
    integer :: lsize, rsize

    call get_arg2(cl_args, lsize, rsize)
    call get_vector_ptr(buffers, 0, origin)
    call get_vector_ptr(buffers, 1, left)
    call get_vector_ptr(buffers, 2, right)
    call seq_split_tri_matrix(origin, left, right, lsize, rsize)

  end subroutine

  recursive subroutine split_rect_matrix(buffers, cl_args) bind(C)
    type(c_ptr), value, intent(in) :: buffers, cl_args
    double precision, pointer, contiguous :: origin(:), left(:), right(:)
    integer :: lsize, rsize, nrow

    call get_arg3(cl_args, lsize, rsize, nrow)
    call get_vector_ptr(buffers, 0, origin)
    call get_vector_ptr(buffers, 1, left)
    call get_vector_ptr(buffers, 2, right)
    call seq_split_rect_matrix(origin, left, right, lsize, rsize, nrow)

  end subroutine

end module