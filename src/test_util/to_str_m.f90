module to_str_m
  implicit none
  private
  integer :: STR_LENGTH = 20
  interface to_str
    module procedure to_str_int
    module procedure to_str_logical
    module procedure to_str_DP
  end interface
  public :: to_str
contains

function to_str_int(i) result(res)
  character(STR_LENGTH) :: str, res, digit
  integer, intent(in) :: i
  ! TODO: 二桁以上の対策
  write(digit, "(i1)") calc_digit(i)
  write(str, "(I"//digit//".1)") i
  ! TODO:trimされない(STR_LENGTH分は確保してしまっている)
  res = trim(str)
end function

function to_str_logical(i) result(res)
  character(STR_LENGTH) :: res
  logical, intent(in) :: i
  write(res, *) i
end function

function to_str_DP(a) result(res)
  character(STR_LENGTH) :: res
  double precision, intent(in) :: a
  ! TODO: 大きすぎる数字への対応
  write(res, '(f6.3)') a
end function

integer function calc_digit(num) result(digit)
  ! TODO:負の数の対策
  integer, value :: num
  if(num == 0)then
    digit = 1
    return
  endif
  digit = 0
  do while(num /= 0)
    num = num / 10
    digit = digit + 1
  enddo
end function
end module