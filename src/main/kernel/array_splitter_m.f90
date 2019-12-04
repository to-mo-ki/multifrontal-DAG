module array_splitter_m
  implicit none
  private

  public :: split_array
  
contains
  subroutine split_array(origin, left, right, lsize, rsize)
    integer, intent(in) :: lsize, rsize
    double precision :: origin(lsize+rsize), left(lsize), right(rsize)
    integer :: i
    
    left = origin(:lsize)
    right = origin(lsize+1:lsize+rsize)

  end subroutine
  
end module