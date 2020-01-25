module disjoint_set_m
  implicit none
  private
  type, public :: disjoint_set_c
    private
    integer, pointer, contiguous :: parent(:), realroot(:), virtualroot(:), rank(:)
  contains
    procedure :: find
    procedure :: find2
    procedure :: link
  end type

  public :: create_disjoint_set
  
contains
  function create_disjoint_set(n) result(this)
    type(disjoint_set_c), pointer :: this
    integer, intent(in) :: n
    integer :: i
    
    allocate(this)
    allocate(this%parent(n), this%realroot(n), this%virtualroot(n), this%rank(n))
    do i=1, n
      this%parent(i) = i
      this%realroot(i) = i
      this%virtualroot(i) = i
      this%rank(i) = 0
    enddo

  end function

  integer function find(this, idx) result(root)
    class(disjoint_set_c) :: this
    integer, intent(in) :: idx
    if(this%parent(idx) == idx)then
      root = this%realroot(idx)
      return
    endif
    root = this%find2(this%parent(idx))
    this%parent(idx) = root
    root = this%realroot(root)

  end function

  recursive integer function find2(this, idx) result(root)
    class(disjoint_set_c) :: this
    integer, intent(in) :: idx
    if(this%parent(idx) == idx)then
      root = idx
      return
    endif
    root = this%find2(this%parent(idx))
    this%parent(idx) = root
  end function

  subroutine link(this, node, parent)
    class(disjoint_set_c) :: this
    integer, intent(in) :: node, parent
    integer :: node_vroot, parent_vroot
    
    node_vroot = this%virtualroot(node)
    parent_vroot = this%virtualroot(parent)
    if(this%rank(parent_vroot) > this%rank(node_vroot))then
      this%parent(node_vroot) = parent_vroot
    else if(this%rank(parent_vroot) < this%rank(node_vroot))then
      this%parent(parent_vroot) = node_vroot
      this%realroot(node_vroot) = this%realroot(parent_vroot)
      this%virtualroot(parent) = node_vroot
    else
      this%parent(node_vroot) = parent_vroot
      this%rank(parent_vroot) = this%rank(parent_vroot) + 1
    endif
    

  end subroutine
end module