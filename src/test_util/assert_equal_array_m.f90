module assert_equal_array_m
  use to_str_m, only: to_str
  use log_m
  implicit none
  private
  interface assert_equal_array
    module procedure assert_equal_array_DP
    module procedure assert_equal_array_int
  end interface
  public :: assert_equal_array, assert_equal_partial_array
contains
  subroutine assert_equal_array_DP(message, answer, n, check)
    character(*), intent(in) :: message
    double precision, intent(in) :: answer(:), check(:)
    integer, intent(in) :: n
    logical :: err_flag_array(n), err_flag
    integer :: i
    err_flag = .false.
    err_flag_array = .false.
    do i=1,n
      if(answer(i) /= check(i))then
        err_flag = .true.
        err_flag_array(i) = .true.
      endif
    enddo

    if(err_flag)then
      call fail_message(message)
    else
      call success_message(message)
      return
    endif

    do i=1,n
      if(err_flag_array(i))then
        write(*,*) "  ", trim(to_str(i)), "-th element ", trim(to_str(answer(i))),&
        & ' is NOT EQUAL to ', trim(to_str(check(i)))
      endif
    enddo

  end subroutine

  subroutine assert_equal_array_int(message, answer, n, check)
    character(*), intent(in) :: message
    integer, intent(in) :: answer(:), check(:)
    integer, intent(in) :: n
    logical :: err_flag_array(n), err_flag
    integer :: i
    err_flag = .false.
    err_flag_array = .false.
    do i=1,n
      if(answer(i) /= check(i))then
        err_flag = .true.
        err_flag_array(i) = .true.
      endif
    enddo

    if(err_flag)then
      call fail_message(message)
    else
      call success_message(message)
      return
    endif

    do i=1,n
      if(err_flag_array(i))then
        write(*,*) "  ", trim(to_str(i)), "-th element ", trim(to_str(answer(i))),&
        & ' is NOT EQUAL to ', trim(to_str(check(i)))
      endif
    enddo

  end subroutine

  subroutine assert_equal_partial_array(message, answer, pos, n, check, precision)
    character(*), intent(in) :: message
    double precision, intent(in) :: answer(*), check(n)
    integer, intent(in) :: n, pos(*)
    double precision, optional :: precision
    logical :: err_flag_array(n), err_flag
    integer :: i
    double precision :: ignore_digits = 1d-15
    if(present(precision))then
      ignore_digits = precision
    endif
    err_flag = .false.
    err_flag_array = .false.
    do i=1,n
      if(abs(answer(pos(i)) - check(i)) > ignore_digits)then
        err_flag = .true.
        err_flag_array(i) = .true.
      endif
    enddo
    

    if(err_flag)then
      call fail_message(message)
    else
      call success_message(message)
      return
    endif

    do i=1,n
      if(err_flag_array(i))then
        write(*,*) "  ", trim(to_str(pos(i))), "-th element ", trim(to_str(answer(pos(i)))),&
        & ' is NOT EQUAL to ', trim(to_str(check(i)))
      endif
    enddo

  end subroutine
end module