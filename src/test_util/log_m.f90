module log_m
  implicit none
  
contains
  subroutine disp_log(err_flag, message, answer, check)
    logical, intent(in) :: err_flag
    character(*), intent(in) :: message
    character(*), optional :: answer, check
    if (err_flag) then
      call fail_message(message)
      if(present(answer) .and. present(check)) then
        write(*,*) "  ", trim(answer), ' is NOT EQUAL to ', trim(check)
      endif
    else
      call success_message(message)
    end if

  end subroutine

  subroutine fail_message(message)
    character(*), intent(in) :: message
    call update_count(.true.)
    write(*,"(a, a, a)") ' *** Error [AssertEQ] *** Checking ', trim(message), ' FAILURE'
  end subroutine

  subroutine success_message(message)
    character(*), intent(in) :: message
    call update_count(.false.)
    write(*,"(a, a, a)") ' *** MESSAGE [AssertEQ] *** Checking ', trim(message),  ' OK'
  end subroutine

  subroutine update_count(err_flag)
    logical, intent(in) :: err_flag
    integer:: correct, total, ios

    open(unit=10, iostat=ios, file='.test', action='readwrite', form='formatted', status='old')
    if (ios /= 0) then
      write(*,*) 'Failed to open'
      stop
    end if
    read(10, *) correct, total
    total = total + 1
    if(.not. err_flag)then
      correct = correct + 1
    endif
    rewind(10)
    write(10, *) correct, total
    close(10)
  end subroutine
end module