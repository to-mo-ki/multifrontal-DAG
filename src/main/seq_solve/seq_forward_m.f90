module seq_forward_m
  use node_data_m
  use factors_m
  use right_hand_m
  use jagged_array_3D_m
  use block_local_index_info_m
  implicit none
  private
  
  public :: seq_forward
  
contains

  subroutine seq_forward(node_data, factors, rh, block_local_index, block_local_index_info, parent)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, contiguous :: parent(:)
    integer :: node

    do node=1, node_data%num_node
      call supernode_forward(node_data, factors, rh, node)
      !TODO: TEST
      if(node_data%exist_border(node))then
        call border_forward2(node_data, factors, rh, node)
      endif
      call sparse_add(rh, block_local_index, block_local_index_info, node, parent(node))
    enddo

  end subroutine

  subroutine supernode_forward(node_data, factors, rh, node)
    use solve_subroutines_m
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    do j=1, node_data%get_work_start_index(node)-1
      call forward(node_data, factors, rh, node, j)
      do i=j+1, node_data%get_num_matrix_block(node)
        call update_l(node_data, factors, rh, node, i, j)
      enddo
    enddo

  end subroutine

  subroutine border_forward2(node_data, factors, rh, node)
    use border_solve_subroutines_m
    use array_rearrange_subroutines_m
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    j = node_data%get_work_start_index(node)
    call rearrange_array_f(rh, node, j)
    call border_forward(node_data, factors, rh, node, j)
    do i=j+1, node_data%get_num_matrix_block(node)
      call border_update_l(node_data, factors, rh, node, i, j)
    enddo
    
  end subroutine

  subroutine sparse_add(rh, block_local_index, block_local_index_info, node, parent_node)
    use sparse_add_subroutines_m
    type(right_hand_c), pointer :: rh
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, intent(in) :: node, parent_node
    integer :: i

    do i=1, block_local_index_info%get_num_block(node)
      call scatter_add(rh, block_local_index, block_local_index_info, i, node, parent_node)
    enddo

  end subroutine

end module