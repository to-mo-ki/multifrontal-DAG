module sort_m
  implicit none
  private

  public :: sort, sort_with_perm

contains

  subroutine sort(a)
    integer, intent(inout) :: a(:)
    integer :: i, j, tmp

    do i=1,size(a)
      do j=size(a), i+1, -1
        if(a(j-1) > a(j))then
          call swap(a(j-1), a(j))
        end if
      end do
    enddo

  end subroutine sort

  subroutine sort_with_perm(a, perm)
    integer, intent(inout) :: a(:), perm(:)
    integer :: i, j, tmp

    do i=1, size(a)
      do j=size(a), i+1, -1
        if(a(j-1) > a(j))then
          call swap(a(j-1), a(j))
          call swap(perm(j-1), perm(j))
        end if
      end do
    end do

  end subroutine

  subroutine swap(a, b)
    integer, intent(inout) :: a, b
    integer :: tmp

    tmp = a
    a = b
    b = tmp

  end subroutine swap

end module sort_m
