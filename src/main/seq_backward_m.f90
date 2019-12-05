module seq_backward_m
  use factors_m
  use right_hand_m
  use block_local_index_m
  implicit none
  private
  
  public :: seq_backward
  
contains

  subroutine seq_backward(factors, rh, block_local_index, parent)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(block_local_index_c), pointer :: block_local_index
    integer, contiguous :: parent(:)
    integer :: node, i, j

    do node=factors%get_num_node(), 1, -1
      call sparse_add(factors, rh, block_local_index, node, parent(node))
      if(factors%exist_border(node))then
        call border_backward2(factors, rh, node)
      endif
      call supernode_backward(factors, rh, node)
    enddo

  end subroutine

  subroutine supernode_backward(factors, rh, node)
    use solve_subroutines_m
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    do j=factors%get_work_start_index(node)-1, 1, -1
      do i=j+1, factors%get_num_block(node)
        call update_u(factors, rh, node, i, j)
      enddo
      call backward(factors, rh, node, j)
    enddo

  end subroutine

  subroutine border_backward2(factors, rh, node)
    use border_solve_subroutines_m
    use array_rearrange_subroutines_m
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    j = factors%get_work_start_index(node)
    do i=j+1, factors%get_num_block(node)
      call border_update_u(factors, rh, node, i, j)
    enddo
    call border_backward(factors, rh, node, j)
    call rearrange_array_b(rh, node, j)
    
  end subroutine

  subroutine sparse_add(factors, rh, block_local_index, node, parent_node)
    use sparse_add_subroutines_m
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(block_local_index_c), pointer :: block_local_index
    integer, intent(in) :: node, parent_node
    integer :: i

    do i=1, block_local_index%get_num_block(node)
      call gather_add(factors, rh, block_local_index, i, node, parent_node)
    enddo

  end subroutine

end module