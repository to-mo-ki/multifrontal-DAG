program block_local_index_info_test
  use jagged_array_m
  use contiguous_sets_m
  use block_local_index_info_m
  use jagged_array_3D_m
  use node_data_m
  use test_util
  implicit none
  type(contiguous_sets_c), pointer :: node_set
  type(jagged_array_c), pointer :: local_index
  type(block_local_index_info_c), pointer :: block_local_index_info
  type(jagged_array_3D_c), pointer :: block_local_index
  type(node_data_c), pointer :: node_data

  call test1
  call test2
  call test3
  call test4

contains
  subroutine test1()
    local_index => create_jagged_array([4, 0], [2, 3, 7, 8])
    node_data => create_node_data([5, 8], [4,0], 3)
    block_local_index_info => create_block_local_index_info(node_data, local_index)
    block_local_index => block_local_index_info%create_block_local_index()

    call start_array_tests("local_index")
    call add_test("local_index:node=1, block_num=1", block_local_index%get_array(1, 1), [2])
    call add_test("local_index:node=1, block_num=2", block_local_index%get_array(1, 2), [3])
    call add_test("local_index:node=1, block_num=3", block_local_index%get_array(1, 3), [1, 2])
    call end_array_tests()

    call start_tests("start_row_num")
    call add_test(1, block_local_index_info%get_start_row_num(1, 1), 1)
    call add_test(2, block_local_index_info%get_start_row_num(1, 2), 2)
    call add_test(3, block_local_index_info%get_start_row_num(1, 3), 3)
    call end_tests()

    call start_tests("block_offset")
    call add_test("node=1, block_num=1", block_local_index_info%get_block_offset(1, 1), 0)
    call add_test("node=1, block_num=2", block_local_index_info%get_block_offset(1, 2), 0)
    call add_test("node=1, block_num=3", block_local_index_info%get_block_offset(1, 3), 1)
    call end_tests()

    call start_tests("parent")
    call add_test("node=1,idx=1", block_local_index_info%get_parent_num(1, 1), 1)
    call add_test("node=1,idx=2", block_local_index_info%get_parent_num(1, 2), 1)
    call add_test("node=1,idx=3", block_local_index_info%get_parent_num(1, 3), 3)
    call end_tests()

    call start_tests("child")
    call add_test("node=1,idx=1", block_local_index_info%get_child_num(1, 1), 2)
    call add_test("node=1,idx=2", block_local_index_info%get_child_num(1, 2), 3)
    call add_test("node=1,idx=3", block_local_index_info%get_child_num(1, 3), 3)
    call end_tests()

    call start_tests("num_blocks")
    call add_test("node=1", block_local_index_info%get_num_block(1), 3)
    call add_test("node=2", block_local_index_info%get_num_block(2), 0)
    call end_tests()
    
  end subroutine

  subroutine test2()
    local_index => create_jagged_array([3, 2, 2, 1, 0], [2, 4, 6, 1, 5, 2, 3, 1])
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 2, 1, 0], 2)
    block_local_index_info => create_block_local_index_info(node_data, local_index)
    block_local_index => block_local_index_info%create_block_local_index()

    call start_array_tests("local_index")
    call add_test("node=1, block_num=1", block_local_index%get_array(1, 1), [2])
    call add_test("node=1, block_num=2", block_local_index%get_array(1, 2), [2])
    call add_test("node=1, block_num=3", block_local_index%get_array(1, 3), [2])
    call add_test("node=2, block_num=1", block_local_index%get_array(2, 1), [1])
    call add_test("node=2, block_num=2", block_local_index%get_array(2, 2), [1])
    call add_test("node=3, block_num=1", block_local_index%get_array(3, 1), [2])
    call add_test("node=3, block_num=2", block_local_index%get_array(3, 2), [1])
    call add_test("node=4, block_num=1", block_local_index%get_array(4, 1), [1])
    call end_array_tests()

    call start_tests("start_row_num")
    call add_test("node=1, block_num=1", block_local_index_info%get_start_row_num(1, 1), 1)
    call add_test("node=1, block_num=2", block_local_index_info%get_start_row_num(1, 2), 2)
    call add_test("node=1, block_num=3", block_local_index_info%get_start_row_num(1, 3), 3)
    call add_test("node=2, block_num=1", block_local_index_info%get_start_row_num(2, 1), 1)
    call add_test("node=2, block_num=2", block_local_index_info%get_start_row_num(2, 2), 2)
    call add_test("node=3, block_num=1", block_local_index_info%get_start_row_num(3, 1), 1)
    call add_test("node=3, block_num=2", block_local_index_info%get_start_row_num(3, 2), 2)
    call add_test("node=4, block_num=1", block_local_index_info%get_start_row_num(4, 1), 1)
    call end_tests()

    call start_tests("block_offset")
    call add_test("node=1, block_num=1", block_local_index_info%get_block_offset(1, 1), 0)
    call add_test("node=1, block_num=2", block_local_index_info%get_block_offset(1, 2), 0)
    call add_test("node=1, block_num=3", block_local_index_info%get_block_offset(1, 3), 1)
    call add_test("node=2, block_num=1", block_local_index_info%get_block_offset(2, 1), 0)
    call add_test("node=2, block_num=2", block_local_index_info%get_block_offset(2, 2), 1)
    call add_test("node=3, block_num=1", block_local_index_info%get_block_offset(3, 1), 0)
    call add_test("node=3, block_num=2", block_local_index_info%get_block_offset(3, 2), 1)
    call add_test("node=4, block_num=1", block_local_index_info%get_block_offset(4, 1), 0)
    call end_tests()

    call start_tests("parent")
    call add_test("node=1,idx=1", block_local_index_info%get_parent_num(1, 1), 1)
    call add_test("node=1,idx=2", block_local_index_info%get_parent_num(1, 2), 2)
    call add_test("node=1,idx=3", block_local_index_info%get_parent_num(1, 3), 3)
    call add_test("node=2,idx=1", block_local_index_info%get_parent_num(2, 1), 1)
    call add_test("node=2,idx=2", block_local_index_info%get_parent_num(2, 2), 3)
    call add_test("node=3,idx=1", block_local_index_info%get_parent_num(3, 1), 1)
    call add_test("node=3,idx=2", block_local_index_info%get_parent_num(3, 2), 2)
    call add_test("node=4,idx=1", block_local_index_info%get_parent_num(4, 1), 1)
    call end_tests()

    call start_tests("child")
    call add_test("node=1,idx=1", block_local_index_info%get_child_num(1, 1), 2)
    call add_test("node=1,idx=2", block_local_index_info%get_child_num(1, 2), 3)
    call add_test("node=1,idx=3", block_local_index_info%get_child_num(1, 3), 3)
    call add_test("node=2,idx=1", block_local_index_info%get_child_num(2, 1), 2)
    call add_test("node=2,idx=2", block_local_index_info%get_child_num(2, 2), 2)
    call add_test("node=3,idx=1", block_local_index_info%get_child_num(3, 1), 3)
    call add_test("node=3,idx=2", block_local_index_info%get_child_num(3, 2), 3)
    call add_test("node=4,idx=1", block_local_index_info%get_child_num(4, 1), 1)
    call end_tests()

    call start_tests("num_blocks")
    call add_test("node=1", block_local_index_info%get_num_block(1), 3)
    call add_test("node=2", block_local_index_info%get_num_block(2), 2)
    call add_test("node=1", block_local_index_info%get_num_block(3), 2)
    call add_test("node=2", block_local_index_info%get_num_block(4), 1)
    call add_test("node=2", block_local_index_info%get_num_block(5), 0)
    call end_tests()
    
  end subroutine


  subroutine test3()
    local_index => create_jagged_array([3, 2, 3, 1, 0], [2, 4, 7, 1, 6, 1, 2, 3, 1])
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 3, 1, 0], 2)
    block_local_index_info => create_block_local_index_info(node_data, local_index)
    block_local_index => block_local_index_info%create_block_local_index()

    call start_array_tests("local_index")
    call add_test("node=1, block_num=1", block_local_index%get_array(1, 1), [2])
    call add_test("node=1, block_num=2", block_local_index%get_array(1, 2), [2])
    call add_test("node=1, block_num=3", block_local_index%get_array(1, 3), [1])
    call add_test("node=2, block_num=1", block_local_index%get_array(2, 1), [1])
    call add_test("node=2, block_num=2", block_local_index%get_array(2, 2), [2])
    call add_test("node=3, block_num=1", block_local_index%get_array(3, 1), [1, 2])
    call add_test("node=3, block_num=2", block_local_index%get_array(3, 2), [1])
    call add_test("node=4, block_num=1", block_local_index%get_array(4, 1), [1])
    call end_array_tests()

    call start_tests("start_row_num")
    call add_test("node=1, block_num=1", block_local_index_info%get_start_row_num(1, 1), 1)
    call add_test("node=1, block_num=2", block_local_index_info%get_start_row_num(1, 2), 2)
    call add_test("node=1, block_num=3", block_local_index_info%get_start_row_num(1, 3), 3)
    call add_test("node=2, block_num=1", block_local_index_info%get_start_row_num(2, 1), 1)
    call add_test("node=2, block_num=2", block_local_index_info%get_start_row_num(2, 2), 2)
    call add_test("node=3, block_num=1", block_local_index_info%get_start_row_num(3, 1), 1)
    call add_test("node=3, block_num=2", block_local_index_info%get_start_row_num(3, 2), 3)
    call add_test("node=4, block_num=1", block_local_index_info%get_start_row_num(4, 1), 1)
    call end_tests()

    call start_tests("block_offset")
    call add_test("node=1, block_num=1", block_local_index_info%get_block_offset(1, 1), 0)
    call add_test("node=1, block_num=2", block_local_index_info%get_block_offset(1, 2), 0)
    call add_test("node=1, block_num=3", block_local_index_info%get_block_offset(1, 3), 1)
    call add_test("node=2, block_num=1", block_local_index_info%get_block_offset(2, 1), 0)
    call add_test("node=2, block_num=2", block_local_index_info%get_block_offset(2, 2), 1)
    call add_test("node=3, block_num=1", block_local_index_info%get_block_offset(3, 1), 0)
    call add_test("node=3, block_num=2", block_local_index_info%get_block_offset(3, 2), 0)
    call add_test("node=4, block_num=1", block_local_index_info%get_block_offset(4, 1), 0)
    call end_tests()

    call start_tests("parent")
    call add_test("node=1,idx=1", block_local_index_info%get_parent_num(1, 1), 1)
    call add_test("node=1,idx=2", block_local_index_info%get_parent_num(1, 2), 2)
    call add_test("node=1,idx=3", block_local_index_info%get_parent_num(1, 3), 4)
    call add_test("node=2,idx=1", block_local_index_info%get_parent_num(2, 1), 1)
    call add_test("node=2,idx=2", block_local_index_info%get_parent_num(2, 2), 3)
    call add_test("node=3,idx=1", block_local_index_info%get_parent_num(3, 1), 1)
    call add_test("node=3,idx=2", block_local_index_info%get_parent_num(3, 2), 2)
    call add_test("node=4,idx=1", block_local_index_info%get_parent_num(4, 1), 1)
    call end_tests()

    call start_tests("child")
    call add_test("node=1,idx=1", block_local_index_info%get_child_num(1, 1), 2)
    call add_test("node=1,idx=2", block_local_index_info%get_child_num(1, 2), 3)
    call add_test("node=1,idx=3", block_local_index_info%get_child_num(1, 3), 3)
    call add_test("node=2,idx=1", block_local_index_info%get_child_num(2, 1), 2)
    call add_test("node=2,idx=2", block_local_index_info%get_child_num(2, 2), 2)
    call add_test("node=3,idx=1", block_local_index_info%get_child_num(3, 1), 3)
    call add_test("node=3,idx=2", block_local_index_info%get_child_num(3, 2), 4)
    call add_test("node=4,idx=1", block_local_index_info%get_child_num(4, 1), 1)
    call end_tests()
    
    call start_tests("num_blocks")
    call add_test("node=1", block_local_index_info%get_num_block(1), 3)
    call add_test("node=2", block_local_index_info%get_num_block(2), 2)
    call add_test("node=1", block_local_index_info%get_num_block(3), 2)
    call add_test("node=2", block_local_index_info%get_num_block(4), 1)
    call add_test("node=2", block_local_index_info%get_num_block(5), 0)
    call end_tests()

  end subroutine


  subroutine test4()
    local_index => create_jagged_array([5, 3, 0], [2, 3, 4, 5, 7, 1, 3, 4])
    node_data => create_node_data([4,5,4],[5,2,0],3)
    node_set => create_contiguous_sets([4, 5, 4])
    block_local_index_info => create_block_local_index_info(node_data, local_index)
    block_local_index => block_local_index_info%create_block_local_index()

    call start_array_tests("local_index")
    call add_test("node=1, block_num=1", block_local_index%get_array(1, 1), [2, 3])
    call add_test("node=1, block_num=2", block_local_index%get_array(1, 2), [1, 2])
    call add_test("node=1, block_num=3", block_local_index%get_array(1, 3), [1])
    call add_test("node=2, block_num=1", block_local_index%get_array(2, 1), [1])
    call add_test("node=2, block_num=2", block_local_index%get_array(2, 2), [3])
    call add_test("node=2, block_num=3", block_local_index%get_array(2, 3), [1])
    call end_array_tests()
    
    !TODO: バグを修正したのちにget_start_row_numのテスト作成
    call start_tests("block_offset")
    call add_test("node=1, block_num=1", block_local_index_info%get_block_offset(1, 1), 0)
    call add_test("node=1, block_num=2", block_local_index_info%get_block_offset(1, 2), 0)
    call add_test("node=1, block_num=3", block_local_index_info%get_block_offset(1, 3), 2)
    call add_test("node=2, block_num=1", block_local_index_info%get_block_offset(2, 1), 0)
    call add_test("node=2, block_num=2", block_local_index_info%get_block_offset(2, 2), 0)
    call add_test("node=2, block_num=3", block_local_index_info%get_block_offset(2, 3), 1)
    call end_tests()
    
    call start_tests("num_blocks")
    call add_test("node=1", block_local_index_info%get_num_block(1), 3)
    call add_test("node=2", block_local_index_info%get_num_block(2), 3)
    call add_test("node=1", block_local_index_info%get_num_block(3), 0)
    call end_tests()
  end subroutine

  
end program block_local_index_info_test