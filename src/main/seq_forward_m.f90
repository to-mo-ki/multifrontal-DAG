module seq_forward_m
  use factors_m
  use right_hand_m
  use block_local_index_m
  implicit none
  private
  
  public :: seq_forward
  
contains

  subroutine seq_forward(factors, rh, block_local_index, parent)
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(block_local_index_c), pointer :: block_local_index
    integer, contiguous :: parent(:)
    integer :: node, i, j

    do node=1, factors%get_num_node()
      call supernode_forward(factors, rh, node)
      if(factors%exist_border(node))then
        call border_forward2(factors, rh, node)
      endif
      call sparse_add2(factors, rh, block_local_index, node, parent(node))
    enddo

  end subroutine

  subroutine supernode_forward(factors, rh, node)
    use solve_subroutines_m
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    do j=1, factors%get_work_start_index(node)-1
      call forward(factors, rh, node, j)
      do i=j+1, factors%get_num_block(node)
        call update_l(factors, rh, node, i, j)
      enddo
    enddo

  end subroutine

  subroutine border_forward2(factors, rh, node)
    use border_solve_subroutines_m
    use array_rearrange_subroutines_m
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node
    integer :: i, j

    j = factors%get_work_start_index(node)
    call rearrange_array(rh, node, j)
    call border_forward(factors, rh, node, j)
    do i=j+1, factors%get_num_block(node)
      call border_update_l(factors, rh, node, i, j)
    enddo
    
  end subroutine

  subroutine sparse_add2(factors, rh, block_local_index, node, parent_node)
    use sparse_add_subroutines_m
    type(factors_c), pointer :: factors
    type(right_hand_c), pointer :: rh
    type(block_local_index_c), pointer :: block_local_index
    integer, intent(in) :: node, parent_node
    integer :: i

    do i=1, block_local_index%get_num_block(node)
      call scatter_add(factors, rh, block_local_index, i, node, parent_node)
    enddo

  end subroutine

end module