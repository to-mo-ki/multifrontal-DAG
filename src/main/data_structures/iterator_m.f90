module iterator_m
  ! OPTIMIZE: iteratorをポインタで渡すのは毎回allocateが発生するのでよくないかもしれない
  implicit none
  private
  type, public :: iterator_c
    private
    integer :: node
    integer, pointer, contiguous :: next_node(:)
  contains
    procedure :: has_next
    procedure :: next
  end type

  public :: create_iterator
  
contains

  function create_iterator(node, next_node) result(this)
    type(iterator_c), pointer :: this
    integer, intent(in) :: node
    integer, pointer, contiguous, intent(in) :: next_node(:)

    allocate(this)
    this%node = node
    this%next_node => next_node

  end function
  
  logical function has_next(this)
    class(iterator_c) :: this

    has_next = this%node /= 0

  end function has_next

  integer function next(this)
    class(iterator_c) :: this

    next = this%node
    this%node = this%next_node(this%node)

  end function

end module