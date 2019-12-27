module array_tests_m
  use array_node_m
  use log_m
  use to_str_m
  implicit none
  private
  integer :: STR_LENGTH = 20
  character(:), allocatable :: message_buffer

  public :: start_array_tests, end_array_tests
  public :: add_test5, add_test6, add_test7, add_test_tri

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
      call add_size_error_node(message, "different of array size "//"answer:"//to_str(size(answer))//" check:"//to_str(size(check)))
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
        call add_array_err(to_str(i)//"-th element ", to_str(answer(i)), to_str(check(i)))
      endif
    enddo

  end subroutine

  subroutine add_test6(message, answer, check)
    character(*) :: message
    double precision, contiguous :: answer(:), check(:)
    logical :: err_flag
    integer :: i, n

    if(size(answer) /= size(check))then
      call add_size_error_node(message, "different of array size "//"answer:"//to_str(size(answer))//" check:"//to_str(size(check)))
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
        call add_array_err(to_str(i)//"-th element ", to_str(answer(i)), to_str(check(i)))
      endif
    enddo

  end subroutine

  subroutine add_test7(message, answer, check)
    character(*) :: message
    double precision, contiguous :: answer(:)
    integer, contiguous :: check(:)
    double precision, allocatable :: check_db(:)

    allocate(check_db, source=[dble(check)])
    call add_test6(message, answer, check_db)

  end subroutine

  subroutine add_test_tri(message, answer, check, n)
    character(*) :: message
    double precision, contiguous :: answer(:), check(:)
    double precision :: answer2d(n, n)
    logical :: err_flag
    integer :: i, j, n, ptr

    if(size(answer) /= n*n .or. size(check) /= n*(n+1)/2)then
      call add_size_error_node(message, "different of array size "//"n:"//to_str(n)//" answer:"//to_str(size(answer))//" check:"//to_str(size(check)))
      return
    endif

    answer2d = reshape(answer, [n,n])
    
    err_flag = .false.
    ptr = 1
    do i=1, n
      do j=1, i
        if(answer2d(j, i) /= check(ptr))then
          if(.not. err_flag)then
            call add_node(message)
            err_flag = .true.
          endif
          call add_array_err("("//to_str(i)//", "//to_str(j)//")", to_str(answer2d(j,i)), to_str(check(ptr)))
        endif
        ptr = ptr + 1
      enddo
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