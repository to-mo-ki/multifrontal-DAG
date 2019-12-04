module sparse_add_kernel_m
  implicit none
  private

  public :: myaxpyi
contains
  subroutine myaxpyi(x, indx, y, n)
    double precision :: x(*), y(*)
    integer :: indx(*)
    integer, intent(in) :: n
    integer :: i
    
    do i=1,n
      y(indx(i)) = y(indx(i)) + x(i)
    enddo
    
  end subroutine
end module