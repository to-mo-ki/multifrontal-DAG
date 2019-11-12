module to_str_m
  implicit none
  private
  integer :: STR_LENGTH = 20
  public :: to_str
contains

function to_str(a) result(res)
  class(*), intent(in) :: a
  character(:), pointer :: res
  integer :: length
  length = get_len(a)
  allocate(character(len=length)::res)

  select type (p => a)
  type is (integer)
    res = trim(to_str_int(p))
  type is (double precision)
    res = trim(to_str_DP(p))
  type is (logical)
    res = trim(to_str_logical(p))
  end select
  
end function

function get_len(a) result(length)
  class(*), intent(in) :: a
  integer :: length
  length = 20
  select type (a)
  type is (integer)
    length = len_trim(to_str_int(a))
  type is (double precision)
    length = len_trim(to_str_DP(a))
  type is (logical)
    length = len_trim(to_str_logical(a))
  end select
end function

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
  write(res, '(f10.3)') a
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