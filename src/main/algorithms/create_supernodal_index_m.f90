module create_supernodal_index_m
  use contiguous_sets_m
  use jagged_array_m
  implicit none
  
  public :: create_supernodal_index

contains

  function create_supernodal_index(node_sets, a_ccs, l_ccs) result(supernodal_index)
    type(contiguous_sets_c), pointer :: node_sets
    type(jagged_array_c), pointer :: a_ccs, l_ccs
    type(jagged_array_c), pointer :: supernodal_index
    integer :: node, i, j, offset, order
    integer, pointer, contiguous :: l_ccs_array(:), a_ccs_array(:), supernodal_array(:)
    integer, allocatable :: map(:)

    allocate(map(node_sets%get_num_elements()))
    supernodal_index => create_jagged_array(a_ccs%get_set())

    do node=1, node_sets%get_num_sets()
      offset = node_sets%get_first(node)-1
      order = node_sets%get_length(node)
      do j=1, order
        map(offset+j) = j  
      enddo
      l_ccs_array => l_ccs%get_array(node)
      do i=1, size(l_ccs_array)
        map(l_ccs_array(i)) = order+i
      enddo
      do j=node_sets%get_first(node), node_sets%get_last(node)
        a_ccs_array => a_ccs%get_array(j)
        supernodal_array => supernodal_index%get_array(j)
        do i=1, size(a_ccs_array)
          supernodal_array(i) = map(a_ccs_array(i))
        enddo
      enddo
    enddo

  end function

end module