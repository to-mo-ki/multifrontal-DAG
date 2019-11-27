program block_local_index_creator_test
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use block_local_index_creator_m
  use node_data_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: local_index
  type(contiguous_sets_c), pointer :: num_blocks, num_indices, node_set
  type(jagged_array_3D_c), pointer :: block_local_index
  type(node_data_c), pointer :: node_data
  integer, pointer, contiguous :: local_index_val(:)
  integer, pointer, contiguous :: ans(:)
  integer :: i

  call test1
  call test2
  call test3
  
contains
  subroutine test1()
    local_index => create_jagged_array((/4, 0/), (/2, 3, 7, 8/))
    node_data => create_node_data([5, 8], [4,0], 3)
    
    num_blocks => create_num_blocks(node_data, local_index)
    call assert_equal("num_blocks:1", num_blocks%get_length(1), 3)

    num_indices => create_num_indices(node_data, local_index, num_blocks%get_num_elements())
    call assert_equal("num_indices:1", num_indices%get_length(1), 1)
    call assert_equal("num_indices:2", num_indices%get_length(2), 1)
    call assert_equal("num_indices:3", num_indices%get_length(3), 2)

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 3)
    call assert_equal("rebuild_val", local_index_val, (/2, 3, 1, 2/))

  end subroutine

  subroutine test2()
    local_index => create_jagged_array((/3, 2, 2, 1, 0/), (/2, 4, 6, 1, 5, 2, 3, 1/))
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 2, 1, 0], 2)
    
    num_blocks => create_num_blocks(node_data, local_index)
    call start_tests("num_blocks")
    allocate(ans(4), source=(/3, 2, 2, 1/))
    do i=1, 4
      call add_test(i, num_blocks%get_length(i), ans(i)) 
    enddo
    call end_tests()

    num_indices => create_num_indices(node_data, local_index, num_blocks%get_num_elements())
    allocate(ans(8), source=(/1, 1, 1, 1, 1, 1, 1, 1/))
    call start_tests("num_indices")
    do i=1, 8
      call add_test(i, num_indices%get_length(i), ans(i))
    enddo
    call end_tests()

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 2, 1, 1, 2, 1, 1/))

  end subroutine

  subroutine test3()
    local_index => create_jagged_array((/3, 2, 3, 1, 0/), (/2, 4, 7, 1, 6, 1, 2, 3, 1/))
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 3, 1, 0], 2)
    
    num_blocks => create_num_blocks(node_data, local_index)
    call start_tests("num_blocks")
    allocate(ans(4), source=(/3, 2, 2, 1/))
    do i=1, 4
      call add_test(i, num_blocks%get_length(i), ans(i))
    enddo
    call end_tests


    num_indices => create_num_indices(node_data, local_index, num_blocks%get_num_elements())
    allocate(ans(8), source=(/1, 1, 1, 1, 1, 2, 1, 1/))
    call start_tests("num_indices")
    do i=1, 8
      call add_test(i, num_indices%get_length(i), ans(i))
    enddo
    call end_tests

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 1, 1, 2, 1, 2, 1, 1/))

  end subroutine

  

end program block_local_index_creator_test