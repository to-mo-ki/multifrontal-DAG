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
    type(jagged_array_c), pointer :: lca
    type(disjoint_set_c), pointer :: disjoint_set
    integer, pointer, contiguous, intent(in) :: parent(:)
    integer :: n, p, pp, i, j
    integer, allocatable :: wt(:), prev_p(:)
    integer, pointer, contiguous :: childs(:), rows(:), lca_rows(:)

    n = size(parent)

    disjoint_set => create_disjoint_set(n)

    lca => create_jagged_array(ccs%get_set())
    allocate(prev_p(n))
    prev_p = 0
    do j=1, n-1
      rows => ccs%get_array(j)
      lca_rows => lca%get_array(j)
      do i=2, size(rows)
        pp = prev_p(rows(i))
        if(pp /= 0)then
          lca_rows(i) = disjoint_set%find(pp)
        else
          lca_rows(i) = 0
        endif
        prev_p(rows(i)) = j
      enddo
      call disjoint_set%link(j, parent(j))
    enddo
    allocate(wt(n))
    do i=1, n
      if(tree_child%get_array_length(i) == 0)then
        wt(i) = 1
      else
        wt(i) = 0
      endif
    enddo

    do j=1, n
      if(j /= n)then
        wt(parent(j)) = wt(parent(j)) - 1
      endif
      rows => ccs%get_array(j)
      lca_rows => lca%get_array(j)
      do i=2, size(rows)
        wt(j) = wt(j) + 1
        if(lca_rows(i) /= 0)then
          wt(lca_rows(i)) = wt(lca_rows(i)) - 1
        endif
      enddo
    enddo

    allocate(cc(n))
    cc = wt
    do i=1, n-1
      cc(parent(i)) = cc(parent(i)) + cc(i)
    enddo
    
  end function

end module