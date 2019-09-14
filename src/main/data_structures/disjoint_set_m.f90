module disjoint_set_m
  implicit none
  private
  type, public :: disjoint_set_c
    private
    integer, pointer, contiguous :: parent(:)
  contains
    procedure :: find
    procedure :: link
  end type

  public :: create_disjoint_set
  
contains
  function create_disjoint_set(n) result(this)
    type(disjoint_set_c), pointer :: this
    integer, intent(in) :: n
    integer :: i
    
    allocate(this)
    allocate(this%parent(n))
    do i=1, n
      this%parent(i) = i
    enddo

  end function

  recursive integer function find(this, idx) result(root)
    class(disjoint_set_c) :: this
    integer, intent(in) :: idx
    if(this%parent(idx) == idx)then
      root = idx
      return
    endif
    root = this%find(this%parent(idx))
    this%parent(idx) = root

  end function

  subroutine link(this, node, parent)
    class(disjoint_set_c) :: this
    integer, intent(in) :: node, parent

    this%parent(node) = parent

  end subroutine
end module