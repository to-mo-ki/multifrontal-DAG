module array_tests_m
  use array_node_m
  use log_m
  use to_str_m
  implicit none
  private
  integer :: STR_LENGTH = 20
  character(:), allocatable :: message_buffer

  public :: start_array_tests, end_array_tests
  public :: add_test5

contains

  subroutine start_array_tests(message)
    character(*) :: message
    allocate(message_buffer, source=message)
    message_buffer = message
    call reset_node()
  end subroutine

  subroutine add_test5(message, answer, check)
    character(*) :: message
    integer, contiguous :: answer(:), check(:)
    logical :: err_flag
    integer :: i, n

    if(size(answer) /= size(check))then
      call add_size_error_node(message, size(answer), size(check))
      return
    endif
    n = size(answer)
    err_flag = .false.
    do i=1,n
      if(answer(i) /= check(i))then
        if(.not. err_flag)then
          call add_node(message)
          err_flag = .true.
        endif
        call add_array_err(i, to_str(answer(i)), to_str(check(i)))
      endif
    enddo

  end subroutine

  subroutine end_array_tests()
    if(exist_node())then
      call fail_message(message_buffer)
      call disp_error()
    else
      call success_message(message_buffer)
    endif
    deallocate(message_buffer)
  end subroutine

end module