module relaxed_supernode_m
  use doubly_linked_lists_m
  use jagged_array_m
  use iterator_m
  implicit none
  private

  public :: build_map, create_first_node, create_ccs
  
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

  function create_first_node(first_node_fundamental, map, merge_lists) result(first_node_relaxed)
    integer, pointer, contiguous :: first_node_relaxed(:)
    integer, pointer, contiguous, intent(in) :: first_node_fundamental(:), map(:)
    type(doubly_linked_lists_c), intent(in) :: merge_lists
    type(iterator_c) :: iterator
    integer :: n, num_col, i, node

    n = size(map)
    allocate(first_node_relaxed(n+1))
    first_node_relaxed(1) = 1
    do i=1, n
      iterator = merge_lists%create_iterator(map(i))
      num_col = 0
      do while(iterator%has_next())
        node = iterator%next()
        num_col = num_col + first_node_fundamental(node+1) - first_node_fundamental(node)
      enddo
      first_node_relaxed(i+1) = first_node_relaxed(i) + num_col
    enddo

  end function

  type(jagged_array_c) function create_ccs(map, merge_lists, first_node, ccs_fundamental, order) result(ccs_relaxed)
    ! HACK: サブルーチン化
    use sort_m
    integer, pointer, contiguous :: map(:)
    type(doubly_linked_lists_c), intent(in) :: merge_lists
    integer, pointer, contiguous, intent(in) :: first_node(:)
    type(jagged_array_c), intent(in) :: ccs_fundamental
    integer, intent(in) :: order
    type(iterator_c) :: iterator
    integer, pointer, contiguous :: col(:), row(:), rows_fundamental(:), rows_relaxed(:)
    integer :: n, num_vals, node, num_col, row_num, ptr, num_row, i, j
    integer, allocatable :: full_array(:)

    n = size(map)
    allocate(col(n+1), full_array(order))
    col(1) = 1
    full_array = 0
    do i=1, n
      iterator = merge_lists%create_iterator(map(i))
      num_col = 0
      do while(iterator%has_next())
        node = iterator%next()
        rows_fundamental => ccs_fundamental%get_array(node)
        do j=1, size(rows_fundamental)
          row_num = rows_fundamental(j)
          if(row_num < first_node(i+1) .or. full_array(row_num) == i)then
            cycle
          endif
          num_col = num_col + 1
          full_array(row_num) = i
        enddo
      enddo
      col(i+1) = col(i) + num_col
    enddo

    num_vals = col(n+1)-1
    allocate(row(num_vals))
    
    ccs_relaxed = create_jagged_array(col, row)
    
    full_array = 0
    do i=1, n
      iterator = merge_lists%create_iterator(map(i))
      num_col = 0
      rows_relaxed => ccs_relaxed%get_array(i)
      ptr = 0
      do while(iterator%has_next())
        node = iterator%next()
        rows_fundamental => ccs_fundamental%get_array(node)
        do j=1, size(rows_fundamental)
          row_num = rows_fundamental(j)
          if(row_num < first_node(i+1) .or. full_array(row_num) == i)then
            cycle
          endif
          ptr = ptr + 1
          rows_relaxed(ptr) = row_num
          full_array(row_num) = i
        enddo
      enddo
      num_row = ccs_relaxed%get_array_length(i)
      call sort(rows_relaxed, num_row)
    enddo

  end function

end module