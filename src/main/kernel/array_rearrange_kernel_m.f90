module array_rearrange_kernel_m
  implicit none
  private

  public :: split_array, join_array
  
contains
  subroutine split_array(origin, left, right, lsize, rsize)
    integer, intent(in) :: lsize, rsize
    double precision :: origin(lsize+rsize), left(lsize), right(rsize)
    
    left = left + origin(:lsize)
    right = origin(lsize+1:lsize+rsize)

  end subroutine

  subroutine join_array(dest, left, right, lsize, rsize)
    integer, intent(in) :: lsize, rsize
    double precision :: dest(lsize+rsize), left(lsize), right(rsize)
    
    dest(:lsize) = left
    dest(lsize+1:lsize+rsize) = right

  end subroutine
  
end module