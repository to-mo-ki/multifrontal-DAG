module column_count_m
  use jagged_array_m
  use disjoint_set_m
  implicit none
  private
  public :: column_count
  
contains
  function column_count(ccs, tree_child, parent) result(cc)
    ! TODO: using supernode
    ! TODO: separate least common ancestor
    ! TODO: understand algorithm
    integer, pointer, contiguous :: cc(:)
    type(jagged_array_c), pointer, intent(in) :: ccs, tree_child
    type(disjoint_set_c), pointer :: disjoint_set
    integer, pointer, contiguous, intent(in) :: parent(:)
    integer :: n, p, pp, q, i, u
    integer, allocatable :: wt(:), prev_p(:)
    integer, pointer, contiguous :: level(:)
    integer, pointer, contiguous :: childs(:), rows(:)

    n = size(parent)
    allocate(wt(n), prev_p(n), level(n))

    prev_p = 0
    
    do i=1, n
      if(tree_child%get_array_length(i) == 0)then
        wt(i) = 1
      else
        wt(i) = 0
      endif
    enddo

    level(n) = 0
    childs => tree_child%get_array(n)
    do i=1, size(childs)
      call compute_level(childs(i), n, level, tree_child)
    enddo

    disjoint_set => create_disjoint_set(n)

    do p=1, n
      if(p /= n)then
        wt(parent(p)) = wt(parent(p)) - 1
      endif
      rows => ccs%get_array(p)
      do i=2, size(rows)
        u = rows(i)
        wt(p) = wt(p) + 1
        pp = prev_p(u)
        if(pp /= 0)then
          q = disjoint_set%find(pp)
          wt(q) = wt(q) - 1
        endif
        prev_p(u) = p
      enddo
      call disjoint_set%link(p, parent(p))
    enddo
    allocate(cc(n))
    cc = wt
    do i=1, n-1
      cc(parent(i)) = cc(parent(i)) + cc(i)
    enddo
    
  end function

  recursive subroutine compute_level(node, parent, level, tree_child)
    integer, intent(in) :: node, parent
    integer, pointer, contiguous :: level(:), childs(:)
    type(jagged_array_c), pointer, intent(in) :: tree_child
    integer :: i

    level(node) = level(parent) + 1
    if(tree_child%get_array_length(node) == 0)then
      return
    endif
    childs => tree_child%get_array(node)
    do i=1, size(childs)
      call compute_level(childs(i), node, level, tree_child)
    enddo
  
  end subroutine
end module