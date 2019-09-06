module doubly_linked_lists_m
  use iterator_m, new_iterator => create_iterator
  implicit none
  private
  type, public :: doubly_linked_lists_c
    private
    integer, pointer:: head(:), tail(:), next(:), prev(:)
  contains
    procedure :: add
    procedure :: merge
    procedure :: remove
    procedure :: get_length
    procedure :: get_num_lists
    procedure :: create_iterator
  end type

  public :: create_doubly_linked_lists

contains
  type(doubly_linked_lists_c) function create_doubly_linked_lists(n) result(this)
    integer,intent(in) :: n

    allocate(this%next(n), this%prev(n), this%head(n), this%tail(n))
    this%next = 0
    this%prev = 0
    this%head = 0
    this%tail = 0

  end function

  subroutine add(this, node, idx)
    class(doubly_linked_lists_c) :: this
    integer, intent(in) :: node, idx

    if(this%head(idx) == 0)then
      this%head(idx) = node
      this%tail(idx) = node
    else
      this%prev(node) = this%tail(idx)
      this%next(this%tail(idx)) = node
      this%tail(idx) = node
    endif

  end subroutine add

  subroutine merge(this, from_list, to_list)
    class(doubly_linked_lists_c) :: this
    integer, intent(in) :: from_list, to_list

    if(this%head(from_list) == 0) return
    
    if(this%head(to_list) == 0)then
      this%head(to_list) = this%head(from_list)
      this%tail(to_list) = this%tail(from_list)
    else
      this%next(this%tail(to_list)) = this%head(from_list)
      this%tail(to_list) = this%tail(from_list)
    endif

    this%head(from_list) = 0
    this%tail(from_list) = 0

  end subroutine merge

  subroutine remove(this, node, idx)
    class(doubly_linked_lists_c) :: this
    integer, intent(in) :: node, idx
    integer :: next_node, prev_node

    next_node = this%next(node)
    prev_node = this%prev(node)
    !先頭の場合

    if(node == this%head(idx))then
      this%head(idx) = next_node
    else
      this%next(prev_node) = next_node
    endif
    if(node == this%tail(idx))then
      this%tail(idx) = prev_node
    else
      this%prev(next_node) = prev_node
    endif

    this%next(node) = 0
    this%prev(node) = 0

  end subroutine remove

  integer function get_length(this, node) result(length)
    class(doubly_linked_lists_c) :: this
    type(iterator_c) :: iterator
    integer, intent(in) :: node
    integer :: tmp

    length = 0
    iterator = this%create_iterator(node)
    do while(iterator%has_next())
      length = length + 1
      tmp = iterator%next()
    end do

  end function

  integer function get_num_lists(this) result(num_lists)
    class(doubly_linked_lists_c) :: this
    integer :: i

    num_lists = 0
    do i=1, size(this%head)
      if(this%head(i) /= 0) num_lists = num_lists + 1
    enddo

  end function

  type(iterator_c) function create_iterator(this, idx) result(iterator)
    class(doubly_linked_lists_c) :: this
    integer, intent(in) :: idx
    iterator = new_iterator(this%head(idx), this%next)
  end function

end module
