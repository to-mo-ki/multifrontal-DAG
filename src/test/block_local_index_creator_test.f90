program block_local_index_creator_test
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use block_local_index_creator_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: local_index, jag_2d
  type(jagged_array_c), pointer :: block_num, over
  type(contiguous_sets_c), pointer :: num_blocks, num_indices, node_set
  type(jagged_array_3D_c), pointer :: block_local_index
  integer, pointer, contiguous :: local_index_val(:)
  integer, pointer, contiguous :: ans(:)
  integer :: i

  call test1
  call test2
  call test3
  
contains
  subroutine test1()
    local_index => create_jagged_array((/4, 0/), (/2, 3, 7, 8/))
    node_set => create_contiguous_sets((/5, 8/))
    
    num_blocks => create_num_blocks(local_index, 3)
    call assert_equal("num_blocks:1", num_blocks%get_length(1), 2)

    block_num => create_block_num(local_index, num_blocks, 3)
    call assert_equal("block_num", block_num%get_array(1), (/1, 3/))

    num_indices => create_num_indices(local_index, num_blocks, 3)
    call assert_equal("num_indices:1", num_indices%get_length(1), 2)
    call assert_equal("num_indices:2", num_indices%get_length(2), 2)

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 3)
    call assert_equal("rebuild_val", local_index_val, (/2, 3, 1, 2/))

    jag_2d => create_jagged_array(num_indices, local_index_val)
    block_local_index => create_jagged_array_3D(num_blocks, jag_2d)
    over => create_over(node_set, num_blocks, block_local_index, 3)
    call assert_equal("over", over%get_val(), (/1, 0/))

  end subroutine

  subroutine test2()
    local_index => create_jagged_array((/3, 2, 2, 1, 0/), (/2, 4, 6, 1, 5, 2, 3, 1/))
    node_set => create_contiguous_sets((/3, 2, 4, 1, 3/))

    num_blocks => create_num_blocks(local_index, 2)
    call start_tests("num_blocks")
    allocate(ans(4), source=(/3, 2, 2, 1/))
    do i=1, 4
      call add_test(i, num_blocks%get_length(i), ans(i)) 
    enddo
    call end_tests()

    block_num => create_block_num(local_index, num_blocks, 2)
    call assert_equal("block_num:1", block_num%get_array(1), (/1, 2, 3/))
    call assert_equal("block_num:2", block_num%get_array(2), (/1, 3/))
    call assert_equal("block_num:3", block_num%get_array(3), (/1, 2/))
    call assert_equal("block_num:4", block_num%get_array(4), (/1/))

    num_indices => create_num_indices(local_index, num_blocks, 2)
    
    allocate(ans(8), source=(/1, 1, 1, 1, 1, 1, 1, 1/))
    call start_tests("num_indices")
    do i=1, 8
      call add_test(i, num_indices%get_length(i), ans(i))
    enddo
    call end_tests()

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 2, 1, 1, 2, 1, 1/))

    jag_2d => create_jagged_array(num_indices, local_index_val)
    block_local_index => create_jagged_array_3D(num_blocks, jag_2d)
    over => create_over(node_set, num_blocks, block_local_index, 2)
    call assert_equal("over", over%get_val(), (/0, -1, 0, -1, 0, -1, 0, 0/))

  end subroutine

  subroutine test3()
    local_index => create_jagged_array((/3, 2, 3, 1, 0/), (/2, 4, 7, 1, 6, 1, 2, 3, 1/))
    node_set => create_contiguous_sets((/3, 2, 4, 1, 3/))

    num_blocks => create_num_blocks(local_index, 2)
    
    call start_tests("num_blocks")
    allocate(ans(4), source=(/3, 2, 2, 1/))
    do i=1, 4
      call add_test(i, num_blocks%get_length(i), ans(i))
    enddo
    call end_tests

    block_num => create_block_num(local_index, num_blocks, 2)
    call assert_equal("block_num:1", block_num%get_array(1), (/1, 2, 4/))
    call assert_equal("block_num:2", block_num%get_array(2), (/1, 3/))
    call assert_equal("block_num:3", block_num%get_array(3), (/1, 2/))
    call assert_equal("block_num:4", block_num%get_array(4), (/1/))

    num_indices => create_num_indices(local_index, num_blocks, 2)
    allocate(ans(8), source=(/1, 1, 1, 1, 1, 2, 1, 1/))
    call start_tests("num_indices")
    do i=1, 8
      call add_test(i, num_indices%get_length(i), ans(i))
    enddo
    call end_tests

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 1, 1, 2, 1, 2, 1, 1/))

    jag_2d => create_jagged_array(num_indices, local_index_val)
    block_local_index => create_jagged_array_3D(num_blocks, jag_2d)
    over => create_over(node_set, num_blocks, block_local_index, 2)
    call assert_equal("over", over%get_val(), (/0, -1, 0, -1, 0, 0, -1, 0/))

  end subroutine

  

end program block_local_index_creator_test