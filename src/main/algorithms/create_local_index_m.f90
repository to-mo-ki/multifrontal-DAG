module create_local_index_m
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  private

  public :: create_local_index

contains
  function create_local_index(ccs, node_sets, tree_child) result(local_index)
    type(jagged_array_c), pointer :: ccs, tree_child
    type(contiguous_sets_c), pointer :: node_sets
    type(jagged_array_c), pointer :: local_index
    type(contiguous_sets_c), pointer :: ptr
    integer :: node, num_col, offset, child, i, j
    integer, pointer, contiguous :: rows(:), childs(:), local(:)
    integer, allocatable :: map(:)
    
    allocate(local_index)
    allocate(map(node_sets%get_num_elements()))
    ptr => ccs%get_set()
    local_index => create_jagged_array(ptr)
    do node=1, ccs%get_num_arrays()
      if(tree_child%get_array_length(node) == 0) cycle
      num_col = node_sets%get_length(node)
      offset = node_sets%get_first(node)-1
      do i=1, num_col
        map(offset+i) = i
      enddo
      rows => ccs%get_array(node)
      do i=1, size(rows)
        map(rows(i)) = num_col + i
      enddo
      childs => tree_child%get_array(node)
      do j=1, size(childs)
        child = childs(j)
        local => local_index%get_array(child)
        rows => ccs%get_array(child)
        do i=1, size(local)
          local(i) = map(rows(i))
        enddo
      enddo
    enddo

  end function
  
end module