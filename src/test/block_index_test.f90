program block_index_test
  use block_index_m
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use node_data_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(block_index_c), pointer :: block_index
  integer, pointer, contiguous :: local_index_val(:)
  integer, pointer, contiguous :: ans(:)
  integer :: i

  call test1
  call test2
  call test3
  
contains
  subroutine test1()
    local_index => create_jagged_array([4, 0], [2, 3, 7, 8])
    node_data => create_node_data([5, 8], [4,0], 3)
    block_index => create_block_index(node_data, local_index)
    
    call assert_equal("get_parent", block_index%get_parent_array(1), [1,1,3])
    call assert_equal("get_child", block_index%get_child_array(1), [2,3,3])

  end subroutine

  subroutine test2()
    local_index => create_jagged_array([3, 2, 2, 1, 0], [2, 4, 6, 1, 5, 2, 3, 1])
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 2, 1, 0], 2)
    block_index => create_block_index(node_data, local_index)

    call assert_equal("get_parent:1", block_index%get_parent_array(1), [1,2,3])
    call assert_equal("get_parent:2", block_index%get_parent_array(2), [1,3])
    call assert_equal("get_parent:3", block_index%get_parent_array(3), [1,2])
    call assert_equal("get_parent:4", block_index%get_parent_array(4), [1])

    call assert_equal("get_child:1", block_index%get_child_array(1), [2,3,3])
    call assert_equal("get_child:2", block_index%get_child_array(2), [2,2])
    call assert_equal("get_child:3", block_index%get_child_array(3), [3,3])
    call assert_equal("get_child:4", block_index%get_child_array(4), [1])
    
    
  end subroutine

  subroutine test3()
    local_index => create_jagged_array([3, 2, 3, 1, 0], [2, 4, 7, 1, 6, 1, 2, 3, 1])
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 3, 1, 0], 2)
    block_index => create_block_index(node_data, local_index)

    call assert_equal("get_parent:1", block_index%get_parent_array(1), [1,2,4])
    call assert_equal("get_parent:2", block_index%get_parent_array(2), [1,3])
    call assert_equal("get_parent:3", block_index%get_parent_array(3), [1,2])
    call assert_equal("get_parent:4", block_index%get_parent_array(4), [1])

    call assert_equal("get_child:1", block_index%get_child_array(1), [2,3,3])
    call assert_equal("get_child:2", block_index%get_child_array(2), [2,2])
    call assert_equal("get_child:3", block_index%get_child_array(3), [3,4])
    call assert_equal("get_child:4", block_index%get_child_array(4), [1])

  end subroutine


  
end program block_index_test