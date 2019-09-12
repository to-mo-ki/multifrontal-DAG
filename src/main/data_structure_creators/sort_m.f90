module sort_m
  implicit none

contains

  subroutine sort(a, n)
    integer, intent(inout) :: a(*)
    integer, intent(in) :: n
    integer :: i, j, tmp

    do i=1,n
      do j = n, i+1, -1
        if(a(j) < a(j-1))then
          tmp = a(j)
          a(j) = a(j-1)
          a(j-1) = tmp
        endif
      enddo
    enddo

  end subroutine sort

end module sort_m
