module seq_backward_m
  use node_data_m
  use factors_m
  use right_hand_m
  use jagged_array_3D_m
  use block_local_index_info_m
  implicit none
  private
  
  public :: seq_backward
  
contains

  subroutine seq_backward(node_data, factors, rh, block_local_index, block_local_index_info, parent)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, contiguous :: parent(:)
    integer :: node

    do node=node_data%num_node, 1, -1
      call sparse_add(rh, block_local_index, block_local_index_info, node, parent(node))
      !TODO: TEST
      if(.not. node_data%divisible(node) .and. node /= node_data%num_node)then
        call border_backward2(node_data, factors, rh, node)
      endif
      call supernode_backward(node_data, factors, rh, node)
    enddo

  end subroutine

  subroutine supernode_backward(node_data, factors, rh, node)
    use solve_subroutines_m
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    do j=node_data%get_work_start_index(node)-1, 1, -1
      do i=j+1, node_data%get_num_matrix_block(node)
        call update_u(node_data, factors, rh, node, i, j)
      enddo
      call backward(node_data, factors, rh, node, j)
    enddo

  end subroutine

  subroutine border_backward2(node_data, factors, rh, node)
    use border_solve_subroutines_m
    use array_rearrange_subroutines_m
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    j = node_data%get_work_start_index(node)
    do i=j+1, node_data%get_num_matrix_block(node)
      call border_update_u(node_data, factors, rh, node, i, j)
    enddo
    call border_backward(node_data, factors, rh, node, j)
    call rearrange_array_b(rh, node, j)
    
  end subroutine

  subroutine sparse_add(rh, block_local_index, block_local_index_info, node, parent_node)
    use sparse_add_subroutines_m
    type(right_hand_c), pointer :: rh
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, intent(in) :: node, parent_node
    integer :: i

    do i=1, block_local_index_info%get_num_block(node)
      call gather(rh, block_local_index, block_local_index_info, i, node, parent_node)
    enddo

  end subroutine

end module