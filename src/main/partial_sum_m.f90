module partial_sum_m
  implicit none
  
contains
  integer function partial_sum(s, e)
    integer, intent(in) :: s, e
    partial_sum = e*(e+1)/2-s*(s-1)/2
  end function
end module