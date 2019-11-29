module scalar_tests_m
  use node_m
  use log_m
  use to_str_m
  implicit none
  private
  integer :: STR_LENGTH = 20
  character(:), allocatable :: message_buffer

  public :: start_tests, end_tests
  public :: add_test1, add_test2, add_test3, add_test4

contains

  subroutine start_tests(message)
    character(*) :: message
    allocate(message_buffer, source=message)
    message_buffer = message
    call reset_node()
  end subroutine

  subroutine add_test1(message, answer, check)
    character(*) :: message
    integer :: answer, check
    if(answer /= check)then
      call add_node(message, to_str(answer), to_str(check))
    endif
  end subroutine

  subroutine add_test2(idx, answer, check)
    integer :: idx
    integer :: answer, check
    call add_test1(to_str(idx)//"-th element", answer, check)
  end subroutine

  subroutine add_test3(message, answer, check)
    character(*) :: message
    logical :: answer, check
    if(answer /= check)then
      call add_node(message, to_str(answer), to_str(check))
    endif
  end subroutine

  subroutine add_test4(message, answer, check)
    character(*) :: message
    double precision :: answer, check
    if(answer /= check)then
      call add_node(message, to_str(answer), to_str(check))
    endif
  end subroutine

  subroutine end_tests()
    if(exist_node())then
      call fail_message(message_buffer)
      call disp_error()
    else
      call success_message(message_buffer)
    endif
    deallocate(message_buffer)
  end subroutine

end module