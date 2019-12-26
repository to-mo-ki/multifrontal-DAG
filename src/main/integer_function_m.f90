module integer_function_m
  implicit none
  private

  interface partial_sum
    procedure :: partial_sum1
    procedure :: partial_sum2
  end interface

  public :: partial_sum, div_ceiling, mod2, triangular_pos

contains

  integer function partial_sum1(e)
    integer, intent(in) :: e
    partial_sum1 = e*(e+1)/2
  end function

  integer function partial_sum2(s, e)
    integer, intent(in) :: s, e
    if(e < s)then
      partial_sum2 = 0
    else
      partial_sum2 = partial_sum(e)-partial_sum(s-1)
    endif
  end function

  integer function div_ceiling(a, p)
    integer, intent(in) :: a,p
    div_ceiling = (a-1)/p + 1
  end function

  integer function mod2(a, p)
    integer, intent(in) :: a, p
    mod2 = mod(a-1, p)+1
  end function

  integer function triangular_pos(i, j, n)
    integer, intent(in) :: i, j, n
    triangular_pos = partial_sum(n-j+2, n) + i-j + 1
  end function

end module