module stack_m
  implicit none
  private
  type, public :: stack_c
    private
    integer :: ptr
    integer, pointer, contiguous:: val(:)
  contains
    procedure :: push
    procedure :: pop
    procedure :: is_empty
  end type

  public :: create_stack

contains

  type(stack_c) function create_stack(length) result(this)
     integer,intent(in) :: length
     this%ptr = 0
     allocate(this%val(length))
   end function create_stack

  subroutine push(this, val)
    class(stack_c) :: this
    integer, intent(in) :: val
    this%ptr = this%ptr + 1
    this%val(this%ptr) = val
  end subroutine push

  integer function pop(this) result(val)
    class(stack_c) :: this
    val = this%val(this%ptr)
    this%ptr = this%ptr - 1
  end function

  logical function is_empty(this)
    class(stack_c) :: this
    is_empty = this%ptr == 0
  end function

end module stack_m
