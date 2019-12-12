module relaxed_supernode_m
  use doubly_linked_lists_m
  use jagged_array_m
  use contiguous_sets_m
  use iterator_m
  use stack_m
  implicit none
  private

  public :: build_map, create_node_sets, create_perm
  public :: count_num_child, build_cc
  
contains
  ! merge_listへのポインタ
  function build_map(merge_lists) result(map)
    integer, pointer, contiguous :: map(:)
    type(doubly_linked_lists_c), pointer, intent(in) :: merge_lists
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
    type(contiguous_sets_c), pointer, intent(in) :: node_sets_fundamental, node_sets_relaxed
    type(doubly_linked_lists_c), pointer, intent(in) :: merge_lists
    integer, pointer, contiguous, intent(in) :: map(:)
    type(iterator_c), pointer :: iterator
    integer :: i, j, num_relaxed, node, order, ptr
    type(stack_c), pointer :: stack

    num_relaxed = node_sets_relaxed%get_num_sets()
    order = node_sets_relaxed%get_num_elements()
    allocate(perm(order))
    stack => create_stack(node_sets_fundamental%get_num_sets())

    ptr = 1
    do i=1, num_relaxed
      iterator => merge_lists%create_iterator(map(i))
      do while(iterator%has_next())
        call stack%push(iterator%next())
      enddo
      do while(.not. stack%is_empty())
        node = stack%pop()
        do j=node_sets_fundamental%get_first(node), node_sets_fundamental%get_last(node)
          perm(ptr) = j
          ptr = ptr + 1
        enddo
      enddo
    enddo

  end function

  function create_node_sets(node_sets_fundamental, map, merge_lists) result(node_sets_relaxed)
    type(contiguous_sets_c), pointer :: node_sets_relaxed
    type(contiguous_sets_c), pointer, intent(in) :: node_sets_fundamental
    integer, pointer, contiguous, intent(in) :: map(:)
    type(doubly_linked_lists_c), pointer, intent(in) :: merge_lists
    type(iterator_c), pointer :: iterator
    integer :: n, i, node
    integer, allocatable :: num_col(:)
    
    n = size(map)
    allocate(num_col(n))
    num_col = 0
    do i=1, n
      iterator => merge_lists%create_iterator(map(i))
      do while(iterator%has_next())
        node = iterator%next()
        num_col(i) = num_col(i) + node_sets_fundamental%get_length(node)
      enddo
    enddo
    node_sets_relaxed => create_contiguous_sets(num_col)

  end function

  function count_num_child(sons, map) result(num_child)
    integer, pointer, contiguous :: num_child(:)
    type(doubly_linked_lists_c), pointer, intent(in) :: sons
    integer, pointer, contiguous :: map(:)
    integer :: i, n

    n = size(map)
    allocate(num_child(n))
    do i=1, n
      num_child(i) = sons%get_length(map(i))
    enddo

  end function

  function build_cc(cc_fundamental, map) result(cc_relaxed)
    integer, pointer, contiguous :: cc_relaxed(:)
    integer, pointer, contiguous, intent(in) :: cc_fundamental(:), map(:)
    integer :: n, i

    n = size(map)
    allocate(cc_relaxed(n))
    do i=1, n
      cc_relaxed(i) = cc_fundamental(map(i))
    enddo

  end function

end module