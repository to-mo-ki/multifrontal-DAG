module extend_m
  implicit none

  type, abstract :: super_c
  contains
    procedure(method_i), deferred :: super_method
  end type

  interface 
    integer function method_i(this)
      import super_c
      class(super_c) :: this
    end function
  end interface

  type, extends(super_c) :: sub1_c
    integer :: i = 0
  contains
    procedure :: super_method => sub1_method
  end type

  type, extends(super_c) :: sub2_c
    integer :: i = 0
    integer :: j = 0
  contains
    procedure :: super_method => sub2_method
  end type
  

contains

  integer function sub1_method(this)
    class(sub1_c) :: this
    this%i = this%i + 1
    sub1_method = this%i
  end function

  integer function sub2_method(this)
    class(sub2_c) :: this
    this%i = this%i + 10
    this%j = this%j + 1
    sub2_method = this%i + this%j
  end function

end module