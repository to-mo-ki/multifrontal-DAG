module sparse_add_kernel_m
  implicit none
  private

  public :: scatter_add_kernel, gather_add_kernel
contains
  subroutine scatter_add_kernel(x, indx, y, n)
    double precision :: x(*), y(*)
    integer :: indx(*)
    integer, intent(in) :: n
    integer :: i
    
    do i=1,n
      y(indx(i)) = y(indx(i)) + x(i)
    enddo
    
  end subroutine

  subroutine gather_add_kernel(x, indy, y, n)
    double precision :: x(*), y(*)
    integer :: indy(*)
    integer, intent(in) :: n
    integer :: i
    
    do i=1,n
      y(i) = y(i) + x(indy(i))
    enddo
    
  end subroutine

end module