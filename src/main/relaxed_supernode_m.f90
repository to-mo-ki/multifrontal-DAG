module relaxed_supernode_m
  use doubly_linked_lists_m
  use jagged_array_m
  use contiguous_sets_m
  use iterator_m
  implicit none
  private

  public :: build_map, create_node_sets, create_ccs
  
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

  type(jagged_array_c) function create_ccs(map, merge_lists, node_sets, ccs_fundamental, order) result(ccs_relaxed)
    ! HACK: サブルーチン化
    use sort_m
    integer, pointer, contiguous :: map(:)
    type(doubly_linked_lists_c), intent(in) :: merge_lists
    type(contiguous_sets_c), intent(in) :: node_sets
    type(jagged_array_c), intent(in) :: ccs_fundamental
    integer, intent(in) :: order
    type(iterator_c) :: iterator
    integer, pointer, contiguous :: col(:), row(:), rows_fundamental(:), rows_relaxed(:)
    integer :: n, num_vals, node, row_num, ptr, num_row, i, j
    integer, allocatable :: full_array(:), num_col(:)

    n = size(map)
    allocate(num_col(n), full_array(order))
    num_col = 0
    full_array = 0
    do i=1, n
      iterator = merge_lists%create_iterator(map(i))
      do while(iterator%has_next())
        node = iterator%next()
        rows_fundamental => ccs_fundamental%get_array(node)
        do j=1, size(rows_fundamental)
          row_num = rows_fundamental(j)
          if(row_num <= node_sets%get_last(i) .or. full_array(row_num) == i)then
            cycle
          endif
          num_col(i) = num_col(i) + 1
          full_array(row_num) = i
        enddo
      enddo
    enddo
    
    ccs_relaxed = create_jagged_array(num_col)
    
    full_array = 0
    do i=1, n
      iterator = merge_lists%create_iterator(map(i))
      rows_relaxed => ccs_relaxed%get_array(i)
      ptr = 0
      do while(iterator%has_next())
        node = iterator%next()
        rows_fundamental => ccs_fundamental%get_array(node)
        do j=1, size(rows_fundamental)
          row_num = rows_fundamental(j)
          if(row_num <= node_sets%get_last(i) .or. full_array(row_num) == i)then
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