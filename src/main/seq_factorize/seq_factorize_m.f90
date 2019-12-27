module seq_factorize_m
  use node_data_m
  use factors_m
  use block_local_index_info_m
  use jagged_array_3D_m
  implicit none
  private

  public :: seq_factorize
  
contains

  subroutine seq_factorize(node_data, factors, block_local_index, block_local_index_info, parent)
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, contiguous :: parent(:)
    integer :: node

    do node=1, node_data%num_node
      call supernode_factrize(node_data, factors, node)
      if(node_data%exist_border(node))then
        call border_factorize2(node_data, factors, node)
      endif
      call extend_add(node_data, factors, block_local_index, block_local_index_info, node, parent(node))
    enddo
    
  end subroutine

  subroutine supernode_factrize(node_data, factors, node)
    use factorize_subroutines_m
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node
    integer :: i, j, k, num_block

    num_block = node_data%get_num_matrix_block(node)
    do j=1, node_data%get_work_start_index(node)-1
      call factorize(node_data, factors, node, j)
      do i=j+1, num_block
        call solve(node_data, factors, node, i, j)
      enddo
      do i=j+1, num_block
        call sym_update(node_data, factors, node, i, j)
        do k=i+1, num_block
          call update(node_data, factors, node, i, k, j)
        enddo
      enddo
    enddo

  end subroutine

  subroutine border_factorize2(node_data, factors, node)
    use rearrange_subroutines_m
    use border_subroutines_m
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    integer, intent(in) :: node
    integer :: i, j, k, num_block

    j = node_data%get_work_start_index(node)
    num_block = node_data%get_num_matrix_block(node)
    call rearrange_diag(node_data, factors, node, j)
    do i=j+1, num_block
      call rearrange_ndiag(node_data, factors, node, i, j)
    enddo
    call border_factorize(node_data, factors, node, j)
    do i=j+1, num_block
      call border_solve(node_data, factors, node, i, j)
    enddo
    do i=j+1, num_block
      call border_sym_update(node_data, factors, node, i, j)
      do k=i+1, num_block
        call border_update(node_data, factors, node, i, k, j)
      enddo
    enddo

  end subroutine

  subroutine extend_add(node_data, factors, block_local_index, block_local_index_info, node, parent_node)
    use extend_add_subroutines_m
    type(node_data_c), pointer :: node_data
    type(factors_c), pointer :: factors
    type(jagged_array_3D_c), pointer :: block_local_index
    type(block_local_index_info_c), pointer :: block_local_index_info
    integer, intent(in) :: node, parent_node
    integer :: i, j, num_block

    num_block = block_local_index_info%get_num_block(node)
    do j=1, num_block
      call extend_add_diag(node_data, factors, block_local_index, block_local_index_info, j, node, parent_node)
      do i=j+1, num_block
        call extend_add_ndiag(node_data, factors, block_local_index, block_local_index_info, i, j, node, parent_node)
      enddo
    enddo

  end subroutine

end module