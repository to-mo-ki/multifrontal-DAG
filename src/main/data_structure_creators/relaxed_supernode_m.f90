module relaxed_supernode_m
  use doubly_linked_lists_m
  use jagged_array_m
  use contiguous_sets_m
  use iterator_m
  implicit none
  private

  public :: build_map, create_node_sets, create_perm
  
contains
  ! merge_listへのポインタ
  function build_map(merge_lists) result(map)
    integer, pointer, contiguous :: map(:)
    type(doubly_linked_lists_c), intent(in) :: merge_lists
    integer :: n, num_lists, i, ptr

    num_lists = merge_lists%get_num_lists()
    n = merge_lists%get_num_elements()
    allocate(map(num_lists))
    ptr = 1
    do i=1, n
      if(merge_lists%get_length(i) > 0)then
        map(ptr) = i
        ptr = ptr + 1
      endif
    enddo

  end function

  function create_perm(node_sets_fundamental, node_sets_relaxed,  map, merge_lists) result(perm)
    integer, pointer, contiguous :: perm(:)
    type(contiguous_sets_c), intent(in) :: node_sets_fundamental, node_sets_relaxed
    type(doubly_linked_lists_c), intent(in) :: merge_lists
    integer, pointer, contiguous, intent(in) :: map(:)
    type(iterator_c) :: iterator
    integer :: i, j, num_relaxed, node, order, ptr

    num_relaxed = node_sets_relaxed%get_num_sets()
    order = node_sets_relaxed%get_num_elements()
    allocate(perm(order))
    ptr = 1
    do i=1, num_relaxed
      iterator = merge_lists%create_iterator(map(i))
      do while(iterator%has_next())
        node = iterator%next()
        do j=node_sets_fundamental%get_first(node), node_sets_fundamental%get_last(node)
          perm(ptr) = j
          ptr = ptr + 1
        enddo
      enddo
    enddo

  end function

  type(contiguous_sets_c) function create_node_sets(node_sets_fundamental, map, merge_lists) result(node_sets_relaxed)
    type(contiguous_sets_c), intent(in) :: node_sets_fundamental
    integer, pointer, contiguous, intent(in) :: map(:)
    type(doubly_linked_lists_c), intent(in) :: merge_lists
    type(iterator_c) :: iterator
    integer :: n, i, node
    integer, allocatable :: num_col(:)
    
    n = size(map)
    allocate(num_col(n))
    num_col = 0
    do i=1, n
      iterator = merge_lists%create_iterator(map(i))
      do while(iterator%has_next())
        node = iterator%next()
        num_col(i) = num_col(i) + node_sets_fundamental%get_length(node)
      enddo
    enddo
    node_sets_relaxed = create_contiguous_sets(num_col)

  end function

end module