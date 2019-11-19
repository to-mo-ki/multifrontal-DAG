program block_local_index_creator_test
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use block_local_index_creator_m
  use node_data_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: local_index, jag_2d
  type(jagged_array_c), pointer :: block_num, state
  type(contiguous_sets_c), pointer :: num_blocks, num_indices, node_set
  type(jagged_array_3D_c), pointer :: block_local_index
  integer, pointer, contiguous :: local_index_val(:)

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
    state => create_state(node_set, num_blocks, block_local_index, 3)
    call assert_equal("state", state%get_val(), (/1, 2/))

  end subroutine

  subroutine test2()
    local_index => create_jagged_array((/3, 2, 2, 1, 0/), (/2, 4, 6, 1, 5, 2, 3, 1/))
    node_set => create_contiguous_sets((/3, 2, 4, 1, 3/))

    num_blocks => create_num_blocks(local_index, 2)
    call assert_equal("num_blocks:1", num_blocks%get_length(1), 3)
    call assert_equal("num_blocks:2", num_blocks%get_length(2), 2)
    call assert_equal("num_blocks:3", num_blocks%get_length(3), 2)
    call assert_equal("num_blocks:4", num_blocks%get_length(4), 1)

    block_num => create_block_num(local_index, num_blocks, 2)
    call assert_equal("block_num:1", block_num%get_array(1), (/1, 2, 3/))
    call assert_equal("block_num:2", block_num%get_array(2), (/1, 3/))
    call assert_equal("block_num:3", block_num%get_array(3), (/1, 2/))
    call assert_equal("block_num:4", block_num%get_array(4), (/1/))

    num_indices => create_num_indices(local_index, num_blocks, 2)
    call assert_equal("num_indices:1", num_indices%get_length(1), 1)
    call assert_equal("num_indices:2", num_indices%get_length(2), 1)
    call assert_equal("num_indices:3", num_indices%get_length(3), 1)
    call assert_equal("num_indices:4", num_indices%get_length(4), 1)
    call assert_equal("num_indices:5", num_indices%get_length(5), 1)
    call assert_equal("num_indices:6", num_indices%get_length(6), 1)
    call assert_equal("num_indices:7", num_indices%get_length(7), 1)
    call assert_equal("num_indices:8", num_indices%get_length(8), 1)

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 2, 1, 1, 2, 1, 1/))

    jag_2d => create_jagged_array(num_indices, local_index_val)
    block_local_index => create_jagged_array_3D(num_blocks, jag_2d)
    state => create_state(node_set, num_blocks, block_local_index, 2)
    call assert_equal("state", state%get_val(), (/2, 0, 2, 0, 2, 0, 2, 2/))

  end subroutine

  subroutine test3()
    local_index => create_jagged_array((/3, 2, 3, 1, 0/), (/2, 4, 7, 1, 6, 1, 2, 3, 1/))
    node_set => create_contiguous_sets((/3, 2, 4, 1, 3/))

    num_blocks => create_num_blocks(local_index, 2)
    call assert_equal("num_blocks:1", num_blocks%get_length(1), 3)
    call assert_equal("num_blocks:2", num_blocks%get_length(2), 2)
    call assert_equal("num_blocks:3", num_blocks%get_length(3), 2)
    call assert_equal("num_blocks:4", num_blocks%get_length(4), 1)

    block_num => create_block_num(local_index, num_blocks, 2)
    call assert_equal("block_num:1", block_num%get_array(1), (/1, 2, 4/))
    call assert_equal("block_num:2", block_num%get_array(2), (/1, 3/))
    call assert_equal("block_num:3", block_num%get_array(3), (/1, 2/))
    call assert_equal("block_num:4", block_num%get_array(4), (/1/))

    num_indices => create_num_indices(local_index, num_blocks, 2)
    call assert_equal("num_indices:1", num_indices%get_length(1), 1)
    call assert_equal("num_indices:2", num_indices%get_length(2), 1)
    call assert_equal("num_indices:3", num_indices%get_length(3), 1)
    call assert_equal("num_indices:4", num_indices%get_length(4), 1)
    call assert_equal("num_indices:5", num_indices%get_length(5), 1)
    call assert_equal("num_indices:6", num_indices%get_length(6), 2)
    call assert_equal("num_indices:7", num_indices%get_length(7), 1)
    call assert_equal("num_indices:8", num_indices%get_length(8), 1)

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 1, 1, 2, 1, 2, 1, 1/))

    jag_2d => create_jagged_array(num_indices, local_index_val)
    block_local_index => create_jagged_array_3D(num_blocks, jag_2d)
    state => create_state(node_set, num_blocks, block_local_index, 2)
    call assert_equal("state", state%get_val(), (/2, 0, 2, 0, 2, 2, 0, 2/))

  end subroutine

  

end program block_local_index_creator_test