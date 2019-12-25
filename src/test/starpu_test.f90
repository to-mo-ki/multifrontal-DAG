program starpu_test
  use iso_c_binding
  use fstarpu_mod
  use test_util
  implicit none
  integer(c_int) :: err
  type(c_ptr) :: g_cl, f_cl
  type(c_ptr), pointer, contiguous :: dh_va(:), dh_vb(:)
  integer, pointer, contiguous :: va(:), vb(:), check(:)
  integer :: i
  
  err = fstarpu_init(c_null_ptr)
  if (err /= 0) then
          stop 1          ! StarPU initialization failure
  end if
  allocate(dh_va(10), dh_vb(10))
  allocate(va(2*10), vb(2*10))
  va = 0
  vb = 0

  f_cl = fstarpu_codelet_allocate()
  call fstarpu_codelet_add_cpu_func(f_cl, C_FUNLOC(f_func))
  call fstarpu_codelet_set_where(f_cl, FSTARPU_CPU)
  call fstarpu_codelet_add_buffer(f_cl, FSTARPU_RW)

  g_cl = fstarpu_codelet_allocate()
  call fstarpu_codelet_add_cpu_func(g_cl, C_FUNLOC(g_func))
  call fstarpu_codelet_set_where(g_cl, FSTARPU_CPU)
  call fstarpu_codelet_add_buffer(g_cl, FSTARPU_R)
  call fstarpu_codelet_add_buffer(g_cl, FSTARPU_R)
  call fstarpu_codelet_add_buffer(g_cl, FSTARPU_W)

  do i=1,10
    call fstarpu_vector_data_register(dh_va(i), 0, c_loc(va(2*i-1:2*i)), 2, c_sizeof(va(2*i-1:2*i)))
    call fstarpu_vector_data_register(dh_vb(i), 0, c_loc(vb(2*i-1:2*i)), 2, c_sizeof(va(2*i-1:2*i)))
  enddo

  do i=2,10
    call fstarpu_insert_task((/f_cl, FSTARPU_RW, dh_va(i), c_null_ptr/))
    call fstarpu_insert_task((/g_cl, FSTARPU_R, dh_va(i), FSTARPU_R, dh_vb(i-1), FSTARPU_W, dh_vb(i), c_null_ptr/))
  enddo

  call fstarpu_task_wait_for_all()
  do i=1,10
    call fstarpu_data_unregister(dh_va(i))
    call fstarpu_data_unregister(dh_vb(i))
  enddo
  call fstarpu_codelet_free(f_cl)
  call fstarpu_codelet_free(g_cl)
  call fstarpu_shutdown()
  allocate(check(20))
  check = (/ ((i+1)/2-1,i=1,20) /)
  call assert_equal("starpu test", vb, check)
contains
recursive subroutine f_func(buffers, cl_args) bind(C)
  use iso_c_binding       ! C interfacing module
  use fstarpu_mod         ! StarPU interfacing module
  implicit none
  type(c_ptr), value, intent(in) :: buffers, cl_args ! cl_args is unused
  integer, pointer, contiguous :: a(:)
  integer :: nx

  nx = fstarpu_vector_get_nx(buffers, 0)
  call c_f_pointer(fstarpu_vector_get_ptr(buffers, 0), a, shape=[nx])
  call f(a)

end subroutine

recursive subroutine g_func(buffers, cl_args) bind(C)
  use iso_c_binding       ! C interfacing module
  use fstarpu_mod         ! StarPU interfacing module
  implicit none
  type(c_ptr), value, intent(in) :: buffers, cl_args ! cl_args is unused
  integer, pointer, contiguous :: a(:), b(:), c(:)
  integer :: nx_va, nx_vb, nx_vc

  nx_va = fstarpu_vector_get_nx(buffers, 0)
  nx_vb = fstarpu_vector_get_nx(buffers, 1)
  nx_vc = fstarpu_vector_get_nx(buffers, 2)
  call c_f_pointer(fstarpu_vector_get_ptr(buffers, 0), a, shape=[nx_va])
  call c_f_pointer(fstarpu_vector_get_ptr(buffers, 1), b, shape=[nx_vb])
  call c_f_pointer(fstarpu_vector_get_ptr(buffers, 2), c, shape=[nx_vc])
  call g(a, b, c)
  c = a + b
  
end subroutine

subroutine g(a, b, c)
  implicit none
  integer, pointer, contiguous, intent(in) :: a(:), b(:)
  integer, pointer, contiguous :: c(:)
  c = a + b
end subroutine

subroutine f(a)
  implicit none
  integer, pointer, contiguous :: a(:)
  a = a + 1
end subroutine

end program starpu_test