module starpu_wrapper_m
  use fstarpu_mod
  implicit none
  private

  interface get_arg
    procedure :: get_arg1
    procedure :: get_arg2
    procedure :: get_arg3
    procedure :: get_arg4
  end interface

  interface get_vector_ptr
    procedure :: get_int_vector_ptr
    procedure :: get_DP_vector_ptr
  end interface

  interface register_vector_data
    procedure :: register_int_vector_data
    procedure :: register_DP_vector_data
  end interface

  public :: starpu_init, starpu_finalize, task_wait_for_all, get_vector_ptr
  public :: register_vector_data, unregister_vector_data, get_arg
  
contains
  subroutine starpu_init()
    integer :: err
    err = fstarpu_init(c_null_ptr)
    if (err /= 0) then
      stop 1
    end if
  end subroutine

  subroutine get_DP_vector_ptr(buffers, i, x)
    type(c_ptr), value, intent(in) :: buffers
    integer(c_int), intent(in) :: i
    double precision, pointer, contiguous :: x(:)
    integer :: nx
  
    nx = fstarpu_vector_get_nx(buffers, i)
    call c_f_pointer(fstarpu_vector_get_ptr(buffers, i), x, shape=[nx])
  
  end subroutine

  subroutine get_int_vector_ptr(buffers, i, x)
    type(c_ptr), value, intent(in) :: buffers
    integer(c_int), intent(in) :: i
    integer, pointer, contiguous :: x(:)
    integer :: nx
  
    nx = fstarpu_vector_get_nx(buffers, i)
    call c_f_pointer(fstarpu_vector_get_ptr(buffers, i), x, shape=[nx])
  
  end subroutine

  subroutine register_DP_vector_data(dh, ptr)
    type(c_ptr) :: dh
    double precision, pointer, contiguous :: ptr(:)
    integer(c_int) :: length
    length = ubound(ptr, 1)-lbound(ptr, 1)+1
    call fstarpu_vector_data_register(dh, 0, c_loc(ptr), length, c_sizeof(ptr(1)))
  end subroutine

  subroutine register_int_vector_data(dh, ptr)
    type(c_ptr) :: dh
    integer, pointer, contiguous :: ptr(:)
    integer(c_int) :: length
    length = ubound(ptr, 1)-lbound(ptr, 1)+1
    call fstarpu_vector_data_register(dh, 0, c_loc(ptr), length, c_sizeof(ptr(1)))
  end subroutine

  subroutine unregister_vector_data(dh)
    type(c_ptr) :: dh
    call fstarpu_data_unregister(dh)
  end subroutine

  subroutine starpu_finalize()
    call fstarpu_shutdown()
  end subroutine

  subroutine task_wait_for_all()
    call fstarpu_task_wait_for_all()
  end subroutine
  
  subroutine get_arg1(cl_args, arg1)
    type(c_ptr), value, intent(in) :: cl_args
    integer, intent(out) :: arg1
    call fstarpu_unpack_arg(cl_args,(/c_loc(arg1)/))
  end subroutine

  subroutine get_arg2(cl_args, arg1, arg2)
    type(c_ptr), value, intent(in) :: cl_args
    integer, intent(out) :: arg1, arg2
    call fstarpu_unpack_arg(cl_args,(/c_loc(arg1), c_loc(arg2)/))
  end subroutine

  subroutine get_arg3(cl_args, arg1, arg2, arg3)
    type(c_ptr), value, intent(in) :: cl_args
    integer, intent(out) :: arg1, arg2, arg3
    call fstarpu_unpack_arg(cl_args,(/c_loc(arg1), c_loc(arg2), c_loc(arg3)/))
  end subroutine

  subroutine get_arg4(cl_args, arg1, arg2, arg3, arg4)
    type(c_ptr), value, intent(in) :: cl_args
    integer, intent(out) :: arg1, arg2, arg3, arg4
    call fstarpu_unpack_arg(cl_args,(/c_loc(arg1), c_loc(arg2), c_loc(arg3), c_loc(arg4)/))
  end subroutine

end module