program doubly_linked_list_test
  use doubly_linked_list_m
  use iterator_m
  use test_util
  implicit none
  type(doubly_linked_list_c), pointer :: list
  type(iterator_c), pointer :: iterator
  integer :: n

  n = 5
  list => create_doubly_linked_list(n)
  call list%add(2)
  call check_list("add 2", [2])
  call list%add(5)
  call check_list("add 5", [5, 2])
  call list%add(3)
  call check_list("add 3", [3, 5, 2])
  call list%remove(3)
  call check_list("remove 3", [5, 2])
  call list%remove(2)
  call check_list("remove 2", [5])
  call list%remove(5)
  call assert_equal("remove 5", list%get_length(), 0)
  
  
contains
  subroutine check_list(message, check)
    character(len=*), intent(in) :: message
    integer, intent(in) :: check(:)
    integer :: i, val(5)

    iterator => list%create_iterator()
    i=0
    do while(iterator%has_next())
      i = i + 1
      val(i) = iterator%next()
    enddo
    call assert_equal(message, val(:i), check)

  end subroutine

  
end program doubly_linked_list_test