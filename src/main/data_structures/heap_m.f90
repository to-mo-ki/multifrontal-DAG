module heap_m
  implicit none
  private
  type, public :: heap_c
    private
    integer, pointer, contiguous :: nodes(:)
    integer :: num_node
    procedure(exchange_i), nopass, pointer :: exchange => null()
  contains
    procedure :: reheap
    procedure, public :: update_top_node
    procedure, public :: get_top_node
    procedure, public :: delete_top_node
    procedure, public :: is_empty
    procedure, public :: set_zero
    procedure, public :: max
    procedure, public :: build_heap
  end type

  interface
    logical function exchange_i(a, b)
      integer, intent(in) :: a, b
    end function
  end interface

  public :: create_min_heap, create_max_heap
  
contains

  logical function min_exchange(a, b)
    integer, intent(in) :: a, b
    min_exchange = a < b
  end function

  logical function max_exchange(a, b)
    integer, intent(in) :: a, b
    max_exchange = a > b
  end function

  function create_min_heap(n) result(heap)
    type(heap_c), pointer :: heap
    integer, intent(in) :: n

    allocate(heap)
    allocate(heap%nodes(n))
    heap%exchange => min_exchange

  end function

  function create_max_heap(n) result(heap)
    type(heap_c), pointer :: heap
    integer, intent(in) :: n

    allocate(heap)
    allocate(heap%nodes(n))
    heap%exchange => max_exchange

  end function

  recursive subroutine reheap(this, idx)
    class(heap_c) :: this
    integer, intent(in) :: idx
    integer :: left, right, smallest

    left = 2*idx
    right = 2*idx+1
    smallest = idx
    if(left <= this%num_node)then
      if(this%exchange(this%nodes(left),  this%nodes(smallest)))then
        smallest = left
      endif
    endif
    if(right <= this%num_node)then
      if(this%exchange(this%nodes(right), this%nodes(smallest)))then
        smallest = right
      endif
    endif
    if(smallest /= idx)then
      call swap(this%nodes, idx, smallest)
      call this%reheap(smallest)
    endif

  end subroutine

  subroutine swap(array, idx1, idx2)
    integer, pointer, contiguous :: array(:)
    integer, intent(in) :: idx1, idx2
    integer :: tmp

    tmp = array(idx1)
    array(idx1) = array(idx2)
    array(idx2) = tmp

  end subroutine

  subroutine set_zero(this, num_node)
    class(heap_c) :: this
    integer, intent(in) :: num_node

    this%nodes(:num_node) = 0
    this%num_node = num_node
  end subroutine

  subroutine update_top_node(this, num)
    class(heap_c) :: this
    integer, intent(in) :: num

    this%nodes(1) = num
    call this%reheap(1)
    
  end subroutine

  integer function get_top_node(this) result(top_node)
    class(heap_c) :: this
    top_node = this%nodes(1)
  
  end function

  subroutine delete_top_node(this)
    class(heap_c) :: this

    this%nodes(1) = this%nodes(this%num_node)
    this%num_node = this%num_node - 1
    call this%reheap(1)
  
  end subroutine

  logical function is_empty(this)
    class(heap_c) :: this

    is_empty = this%num_node == 0
    
  end function

  integer function max(this)
    class(heap_c) :: this
    max = maxval(this%nodes(:this%num_node))
  end function

  subroutine build_heap(this, array)
    class(heap_c) :: this
    integer :: array(:)
    integer :: n, i

    n = size(array)
    this%nodes(:n) = array
    this%num_node = n

    do i=n/2, 1, -1
      call this%reheap(i)
    enddo

  end subroutine

end module