module doubly_linked_list_m
  use iterator_m, new_iterator => create_iterator
  implicit none
  private
  type, public :: doubly_linked_list_c
    private
    integer :: head
    integer, pointer, contiguous:: next(:), prev(:)
  contains
    procedure :: add
    procedure :: remove
    procedure :: get_length
    procedure :: create_iterator
  end type

  public :: create_doubly_linked_list

contains
  function create_doubly_linked_list(n) result(this)
    type(doubly_linked_list_c), pointer :: this
    integer,intent(in) :: n

    allocate(this)
    allocate(this%next(n), this%prev(n))
    this%next = 0
    this%prev = 0
    this%head = 0

  end function

  subroutine add(this, node)
    class(doubly_linked_list_c) :: this
    integer, intent(in) :: node

    if(this%head /= 0)then
      this%prev(this%head) = node
    endif
    this%next(node) = this%head
    this%head = node
    this%prev(node) = 0
    
  end subroutine add

  subroutine remove(this, node)
    class(doubly_linked_list_c) :: this
    integer, intent(in) :: node
    integer :: next_node, prev_node

    next_node = this%next(node)
    prev_node = this%prev(node)
    
    if(prev_node == 0)then
      this%head = next_node
    else
      this%next(prev_node) = next_node
    endif
    if(next_node /= 0)then
      this%prev(next_node) = prev_node
    endif
    
    this%next(node) = 0
    this%prev(node) = 0

  end subroutine remove

  integer function get_length(this) result(length)
    class(doubly_linked_list_c), target :: this
    type(iterator_c), pointer :: iterator
    integer :: tmp

    length = 0
    iterator => this%create_iterator()
    do while(iterator%has_next())
      length = length + 1
      tmp = iterator%next()
    end do

  end function

  function create_iterator(this) result(iterator)
    type(iterator_c), pointer :: iterator
    class(doubly_linked_list_c) :: this

    allocate(iterator)
    iterator => new_iterator(this%head, this%next)

  end function

end module
