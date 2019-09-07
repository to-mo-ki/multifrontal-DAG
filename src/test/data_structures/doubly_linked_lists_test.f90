program doubly_linked_list_test
  use doubly_linked_lists_m
  use iterator_m
  use test_util
  implicit none
  type(doubly_linked_lists_c) :: lists
  type(iterator_c) :: iterator
  integer :: n, i, val(5)

  n = 5
  lists = create_doubly_linked_lists(n)
  call lists%add(1, 2)
  call lists%add(5, 2)
  call lists%add(3, 2)
  call lists%add(2, 2)

  call assert_equal("length after 4 adds", lists%get_length(2), 4)
  call check_list("list after 4 adds", 2, (/1, 5, 3, 2/))
  call lists%remove(5, 2)
  call assert_equal("length after middle remove", lists%get_length(2), 3)
  call check_list("list after middle remove", 2, (/1, 3, 2/))
  call lists%remove(1, 2)
  call assert_equal("length after head remove", lists%get_length(2), 2)
  call check_list("list after head remove", 2, (/3, 2/))
  call lists%remove(2, 2)
  call assert_equal("length after tail remove", lists%get_length(2), 1)
  call check_list("list after tail remove", 2, (/3/))
  call lists%remove(3, 2)
  call assert_equal("length after all remove", lists%get_length(2), 0)

  call lists%merge(2, 4)
  call assert_equal("length after null and null merge(from)", lists%get_length(2), 0)
  call assert_equal("length after null and null merge(to)", lists%get_length(4), 0)
  call lists%add(1, 3)
  call lists%add(2, 3)
  call lists%merge(1, 3)
  call assert_equal("length after merge from null to non-null(from)", lists%get_length(1), 0)
  call assert_equal("length after merge from null to non-null(to)", lists%get_length(3), 2)
  call check_list("list after merge from null to non-null list(to)", 3, (/1, 2/))


  call lists%merge(3, 1)
  call assert_equal("length after merge from non-null to null(from)", lists%get_length(3), 0)
  call assert_equal("length after merge from non-null to null(to)", lists%get_length(1), 2)
  call check_list("list after merge from non-null to null(to)", 1, (/1, 2/))

  call lists%add(3, 3)
  call lists%add(4, 3)
  call lists%merge(3, 1)
  call assert_equal("length after merge from non-null to non-null(from)", lists%get_length(3), 0)
  call assert_equal("length after merge from non-null to non-null(to)", lists%get_length(1), 4)
  call check_list("list after merge from non-null to non-null(to)", 1, (/1, 2, 3, 4/))


  
contains
  subroutine check_list(message_list, idx, list)
    character(len=*), intent(in) :: message_list
    integer, intent(in) :: idx, list(:)

    iterator = lists%create_iterator(2)
    i=0
    do while(iterator%has_next())
      i = i + 1
      val(i) = iterator%next()
    enddo
    call assert_equal(message_list, val(:i), list, i)

  end subroutine

end program doubly_linked_list_test