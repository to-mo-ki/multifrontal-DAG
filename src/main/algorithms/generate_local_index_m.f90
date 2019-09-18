module generate_local_index_m
  use jagged_array_m
  use contiguous_sets_m
  implicit none

  
contains

  function generate_local_index(row_index, node_sets, tree_child) result(local_index)
    type(jagged_array_c), pointer :: local_index
    type(jagged_array_c), pointer, intent(in) :: row_index, tree_child
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    integer, pointer, contiguous :: rows(:), childs(:), locals(:)
    integer :: n, i, j, k, order, col_start, col_end
    integer, allocatable :: full_array(:), num_row(:)

    n = row_index%get_num_arrays()
    ! ジャグ配列のコピーで実装する
    allocate(num_row(n))
    do i=1, n
      num_row(i) = row_index%get_array_length(i)
    enddo
    local_index => create_jagged_array(num_row)

    order = node_sets%get_num_elements()
    allocate(full_array(order))
    
    do k=1, n
      if(tree_child%get_array_length(k) == 0) cycle
      col_start = node_sets%get_first(k)
      col_end = node_sets%get_last(k)
      do i=col_start, col_end
        full_array(i) = i - col_start + 1
      enddo
      rows => row_index%get_array(k)
      do i=1, size(rows)
        full_array(rows(i)) = node_sets%get_length(k) + i
      enddo
      childs => tree_child%get_array(k)
      do j=1, size(childs)
        rows => row_index%get_array(childs(j))
        locals => local_index%get_array(childs(j))
        do i=1, size(rows)
          locals(i) = full_array(rows(i))
        enddo
      enddo
    enddo

  end function
end module