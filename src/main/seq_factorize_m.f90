module seq_factorize_m
  use factors_m
  use block_local_index_m
  implicit none
  private

  public :: seq_factorize
  
contains

  subroutine seq_factorize(factors, block_local_index, parent)
    type(factors_c), pointer :: factors
    type(block_local_index_c), pointer :: block_local_index
    integer, contiguous :: parent(:)
    integer :: node

    do node=1, factors%get_num_node()
      call supernode_factrize(factors, node)
      if(factors%exist_border(node))then
        call border_factorize2(factors, node)
      endif
      call extend_add(factors, block_local_index, node, parent(node))
    enddo
    
  end subroutine

  subroutine supernode_factrize(factors, node)
    use factorize_subroutines_m
    type(factors_c), pointer :: factors
    integer, intent(in) :: node
    integer :: i, j, k, num_block

    num_block = factors%get_num_block(node)
    do j=1, factors%get_work_start_index(node)-1
      call factorize(factors, node, j)
      do i=j+1, num_block
        call solve(factors, node, i, j)
      enddo
      do i=j+1, num_block
        call sym_update(factors, node, i, j)
        do k=i+1, num_block
          call update(factors, node, i, k, j)
        enddo
      enddo
    enddo

  end subroutine

  subroutine border_factorize2(factors, node)
    use rearrange_subroutines_m
    use border_subroutines_m
    type(factors_c), pointer :: factors
    integer, intent(in) :: node
    integer :: i, j, k, num_block, ssize, wsize

    j = factors%get_work_start_index(node)
    num_block = factors%get_num_block(node)
    call rearrange_diag(factors, node, j)
    do i=j+1, num_block
      call rearrange_ndiag(factors, node, i, j)
    enddo
    call border_factorize(factors, node, j)
    do i=j+1, num_block
      call border_solve(factors, node, i, j)
    enddo
    do i=j+1, num_block
      call border_sym_update(factors, node, i, j)
      do k=i+1, num_block
        call border_update(factors, node, i, k, j)
      enddo
    enddo

  end subroutine

  subroutine extend_add(factors, block_local_index, node, parent_node)
    use extend_add_subroutines_m
    type(factors_c), pointer :: factors
    type(block_local_index_c), pointer :: block_local_index
    integer, intent(in) :: node, parent_node
    integer :: i, j, num_block

    num_block = block_local_index%get_num_block(node)
    do j=1, num_block
      call extend_add_diag(factors, block_local_index, j, node, parent_node)
      do i=j+1, num_block
        call extend_add_ndiag(factors, block_local_index, i, j, node, parent_node)
      enddo
    enddo

  end subroutine

end module