module tests_m
  use linked_list_m
  use log_m
  use to_str_m
  implicit none
  integer :: STR_LENGTH = 20
  character(:), allocatable :: message_buffer

  interface add_test
    procedure :: add_test1
    procedure :: add_test2
  end interface

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
      call add_node(message, answer, check)
    endif
  end subroutine

  subroutine add_test2(idx, answer, check)
    integer :: idx
    integer :: answer, check
    call add_test1(to_str(idx)//"-th element", answer, check)
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

  subroutine disp_error()
    type(node_c), pointer :: node
    node => get_first_node()
    do while(associated(node))
      write(*,*) node%message, "  ", trim(to_str(node%answer)), ' is NOT EQUAL to ', trim(to_str(node%check))
      node => node%next
    enddo
  end subroutine
end module