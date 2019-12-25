module task_m
  use iso_c_binding, only: c_ptr, c_loc, c_funloc
  use fstarpu_mod
  implicit none
  private
  integer, parameter, public :: MODE_R = 1
  integer, parameter, public :: MODE_RW = 2
  integer, parameter, public :: MODE_RWC = 3
  type, public :: task_c
    type(c_ptr) :: cl
    type(c_ptr), pointer, contiguous :: task(:)
  contains
    procedure :: insert_task
    procedure :: free
  end type

  interface
    subroutine func_i(buffers, cl_args) bind(C)
      import :: c_ptr
      type(c_ptr), value, intent(in) :: buffers, cl_args
    end subroutine
  end interface

  public :: create_task
contains
  type(task_c) function create_task(modes, num_params, func) result(this)
    integer, intent(in) :: modes(:)
    integer, intent(in) :: num_params
    procedure(func_i) :: func
    integer :: i, offset

    this%cl = create_cl(modes, func)
    allocate(this%task(1 + num_params*3 + size(modes)*2 + 1))
    this%task(1) = this%cl
    this%task(1 + num_params*3 + size(modes)*2 + 1) = c_null_ptr
    do i=1, num_params
      this%task(2+(i-1)*3) = FSTARPU_VALUE
      this%task(2+(i-1)*3+2) = FSTARPU_SZ_INTEGER
    enddo

    offset = 1 + num_params*3
    do i=1, size(modes)
      select case(modes(i))
        case (MODE_R)
          this%task((i-1)*2 + 1 + offset) = FSTARPU_R
        case (MODE_RW)
          this%task((i-1)*2 + 1 + offset) = FSTARPU_RW
        case (MODE_RWC)
          this%task((i-1)*2 + 1 + offset) = FSTARPU_RW .ior. FSTARPU_COMMUTE
        case default
          print *,  "Wrong access mode" // trim(modes(i))
      end select
    enddo

  end function

  type(c_ptr) function create_cl(modes, func) result(cl)
    integer, intent(in) :: modes(:)
    procedure(func_i) :: func
    integer :: i

    cl = fstarpu_codelet_allocate()
    call fstarpu_codelet_add_cpu_func(cl, c_funloc(func))
    do i=1, size(modes)
      select case(modes(i))
        case (MODE_R)
          call fstarpu_codelet_add_buffer(cl, FSTARPU_R)
        case (MODE_RW)
          call fstarpu_codelet_add_buffer(cl, FSTARPU_RW)
        case (MODE_RWC)
          call fstarpu_codelet_add_buffer(cl, FSTARPU_RW .ior. FSTARPU_COMMUTE)
        case default
          print *,  "Wrong access mode" // trim(modes(i))
      end select   
    enddo

  end function

  subroutine insert_task(this, params, buffers)
    class(task_c) :: this
    integer, intent(in) :: params(:)
    type(c_ptr), intent(in) :: buffers(:)
    integer :: i, offset

    do i=1,size(params)
      this%task(1+(i-1)*3+2) = c_loc(params(i))
    enddo

    offset = 1 + size(params)*3
    do i=1, size(buffers)
      this%task(offset + i*2) = buffers(i)
    enddo

    call fstarpu_insert_task(this%task)

  end subroutine

  subroutine free(this)
    class(task_c) :: this
    call fstarpu_codelet_free(this%cl)
  end subroutine

end module