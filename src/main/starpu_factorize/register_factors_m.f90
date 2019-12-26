module register_factors_m
  use factors_m
  use starpu_factors_m
  use node_data_m
  use starpu_wrapper_m
  implicit none
  private

  public :: register_factors
  
contains
  subroutine register_factors(node_data, starpu_factors, factors)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: starpu_factors
    type(factors_c), pointer :: factors

    call register_supernode(node_data, starpu_factors, factors)
    call register_work(node_data, starpu_factors, factors)
    call register_boder(node_data, starpu_factors, factors)

  end subroutine

  subroutine register_supernode(node_data, starpu_factors, factors)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: starpu_factors
    type(factors_c), pointer :: factors
    integer :: node, i, j, jend
    
    do node=1, node_data%num_node
      if(node_data%divisible(node))then
        jend = node_data%get_work_start_index(node) - 1
      else
        jend = node_data%get_work_start_index(node)
      endif
      do j=1, jend
        do i=j, node_data%get_num_matrix_block(node)
          call register_vector_data(starpu_factors%get_supernode(node,i,j), factors%get_supernode(node,i,j))
        enddo
      enddo
    enddo
    
  end subroutine

  subroutine register_border(node_data, starpu_factors, factors)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: starpu_factors
    type(factors_c), pointer :: factors
    integer :: node, i, j
    
    do node=1, node_data%num_node
      if(node_data%divisible(node))then
        cycle
      endif
      j = node_data%get_work_start_index(node)
      do i=j, node_data%get_num_matrix_block(node)
        call register_vector_data(starpu_factors%get_border(node,i,j), factors%get_border(node,i,j))
      enddo
    enddo
    
  end subroutine

  subroutine register_work(node_data, starpu_factors, factors)
    type(node_data_c), pointer :: node_data
    type(starpu_factors_c), pointer :: starpu_factors
    type(factors_c), pointer :: factors
    integer :: node, i, j
    
    do node=1, node_data%num_node
      do j=node_data%get_work_start_index(node), node_data%get_num_matrix_block(node)
        do i=j, node_data%get_num_matrix_block(node)
          call register_vector_data(starpu_factors%get_work(node,i,j), factors%get_work(node,i,j))
        enddo
      enddo
    enddo
    
  end subroutine

end module