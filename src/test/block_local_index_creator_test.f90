program block_local_index_creator_test
  use contiguous_sets_m
  use jagged_array_m
  use block_local_index_creator_m
  use node_data_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(jagged_array_c), pointer :: parent_block_num, parent_block_ptr
  type(contiguous_sets_c), pointer :: parent_ptr

  call test1
  call test2
  
contains
  subroutine test1()
    local_index => create_jagged_array((/4, 0/), (/2, 3, 7, 8/))
    node_data => create_node_data((/5, 8/), (/8, 0/), 3)

    parent_ptr => create_parent_ptr(node_data, local_index)
    call assert_equal("parent_ptr:1", parent_ptr%get_length(1), 2)

    parent_block_num => create_parent_block_num(node_data, local_index, parent_ptr)
    call assert_equal("parent_block_num", parent_block_num%get_array(1), (/1, 3/))

    parent_block_ptr => create_parent_block_ptr(node_data, local_index, parent_ptr)
    call assert_equal("parent_block_ptr", parent_block_ptr%get_array(1), (/1, 3/))
  end subroutine

  subroutine test2()
    local_index => create_jagged_array((/3, 2, 2, 1, 0/), (/2, 4, 6, 1, 6, 2, 3, 1/))
    node_data => create_node_data((/3, 2, 4, 1, 3/), (/3, 2, 2, 1, 0/), 2)

    parent_ptr => create_parent_ptr(node_data, local_index)
    call assert_equal("parent_ptr:1", parent_ptr%get_length(1), 3)
    call assert_equal("parent_ptr:2", parent_ptr%get_length(2), 2)
    call assert_equal("parent_ptr:3", parent_ptr%get_length(3), 2)
    call assert_equal("parent_ptr:4", parent_ptr%get_length(4), 1)

    parent_block_num => create_parent_block_num(node_data, local_index, parent_ptr)
    call assert_equal("parent_block_num:1", parent_block_num%get_array(1), (/1, 2, 3/))
    call assert_equal("parent_block_num:2", parent_block_num%get_array(2), (/1, 3/))
    call assert_equal("parent_block_num:3", parent_block_num%get_array(3), (/1, 2/))
    call assert_equal("parent_block_num:4", parent_block_num%get_array(4), (/1/))

    parent_block_ptr => create_parent_block_ptr(node_data, local_index, parent_ptr)
    call assert_equal("parent_block_ptr:1", parent_block_ptr%get_array(1), (/1, 2, 3/))
    call assert_equal("parent_block_ptr:2", parent_block_ptr%get_array(2), (/4, 5/))
    call assert_equal("parent_block_ptr:3", parent_block_ptr%get_array(3), (/6, 7/))
    call assert_equal("parent_block_ptr:4", parent_block_ptr%get_array(4), (/8/))
  end subroutine

  

end program block_local_index_creator_test