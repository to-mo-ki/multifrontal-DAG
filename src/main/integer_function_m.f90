module integer_function_m
  implicit none
  
contains

  integer function partial_sum(s, e)
    integer, intent(in) :: s, e
    if(e < s)then
      partial_sum = 0
      return
    endif
    partial_sum = e*(e+1)/2-s*(s-1)/2
  end function

  integer function div_ceiling(a, p)
    integer, intent(in) :: a,p
    div_ceiling = (a-1)/p + 1
  end function

  integer function mod2(a, p)
    integer, intent(in) :: a, p
    mod2 = mod(a-1, p)+1
  end function

end module