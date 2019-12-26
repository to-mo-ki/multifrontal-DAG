module register_block_local_index_m
  use jagged_array_3D_m
  use jagged_array_cptr_m
  use starpu_wrapper_m
  implicit none
  private

  public :: register_block_local_index

contains
  subroutine register_block_local_index(starpu_block_local_index, block_local_index)
    type(jagged_array_cptr_c) :: starpu_block_local_index
    type(jagged_array_3D_c) :: block_local_index
    integer :: node, i

    do node=1, block_local_index%get_num_1d()
      do i=1, block_local_index%get_num_2d(node)
        call register_vector_data(starpu_block_local_index%get(node, i), block_local_index%get_array(node, i))
      enddo
    enddo

  end subroutine
end module