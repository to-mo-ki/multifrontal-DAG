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
    
    call start_tests("parent")
    call add_test("node=1,idx=1", block_index%get_parent_num(1, 1), 1)
    call add_test("node=1,idx=2", block_index%get_parent_num(1, 2), 1)
    call add_test("node=1,idx=3", block_index%get_parent_num(1, 3), 3)
    call end_tests()

    call start_tests("child")
    call add_test("node=1,idx=1", block_index%get_child_num(1, 1), 2)
    call add_test("node=1,idx=2", block_index%get_child_num(1, 2), 3)
    call add_test("node=1,idx=3", block_index%get_child_num(1, 3), 3)
    call end_tests()

  end subroutine

  subroutine test2()
    local_index => create_jagged_array([3, 2, 2, 1, 0], [2, 4, 6, 1, 5, 2, 3, 1])
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 2, 1, 0], 2)
    block_index => create_block_index(node_data, local_index)

    call start_tests("parent")
    call add_test("node=1,idx=1", block_index%get_parent_num(1, 1), 1)
    call add_test("node=1,idx=2", block_index%get_parent_num(1, 2), 2)
    call add_test("node=1,idx=3", block_index%get_parent_num(1, 3), 3)
    call add_test("node=2,idx=1", block_index%get_parent_num(2, 1), 1)
    call add_test("node=2,idx=2", block_index%get_parent_num(2, 2), 3)
    call add_test("node=3,idx=1", block_index%get_parent_num(3, 1), 1)
    call add_test("node=3,idx=2", block_index%get_parent_num(3, 2), 2)
    call add_test("node=4,idx=1", block_index%get_parent_num(4, 1), 1)
    call end_tests()

    call start_tests("child")
    call add_test("node=1,idx=1", block_index%get_child_num(1, 1), 2)
    call add_test("node=1,idx=2", block_index%get_child_num(1, 2), 3)
    call add_test("node=1,idx=3", block_index%get_child_num(1, 3), 3)
    call add_test("node=2,idx=1", block_index%get_child_num(2, 1), 2)
    call add_test("node=2,idx=2", block_index%get_child_num(2, 2), 2)
    call add_test("node=3,idx=1", block_index%get_child_num(3, 1), 3)
    call add_test("node=3,idx=2", block_index%get_child_num(3, 2), 3)
    call add_test("node=4,idx=1", block_index%get_child_num(4, 1), 1)
    call end_tests()
    
  end subroutine

  subroutine test3()
    local_index => create_jagged_array([3, 2, 3, 1, 0], [2, 4, 7, 1, 6, 1, 2, 3, 1])
    node_data => create_node_data([3, 2, 4, 1, 3], [3, 2, 3, 1, 0], 2)
    block_index => create_block_index(node_data, local_index)


    call start_tests("parent")
    call add_test("node=1,idx=1", block_index%get_parent_num(1, 1), 1)
    call add_test("node=1,idx=2", block_index%get_parent_num(1, 2), 2)
    call add_test("node=1,idx=3", block_index%get_parent_num(1, 3), 4)
    call add_test("node=2,idx=1", block_index%get_parent_num(2, 1), 1)
    call add_test("node=2,idx=2", block_index%get_parent_num(2, 2), 3)
    call add_test("node=3,idx=1", block_index%get_parent_num(3, 1), 1)
    call add_test("node=3,idx=2", block_index%get_parent_num(3, 2), 2)
    call add_test("node=4,idx=1", block_index%get_parent_num(4, 1), 1)
    call end_tests()


    call start_tests("child")
    call add_test("node=1,idx=1", block_index%get_child_num(1, 1), 2)
    call add_test("node=1,idx=2", block_index%get_child_num(1, 2), 3)
    call add_test("node=1,idx=3", block_index%get_child_num(1, 3), 3)
    call add_test("node=2,idx=1", block_index%get_child_num(2, 1), 2)
    call add_test("node=2,idx=2", block_index%get_child_num(2, 2), 2)
    call add_test("node=3,idx=1", block_index%get_child_num(3, 1), 3)
    call add_test("node=3,idx=2", block_index%get_child_num(3, 2), 4)
    call add_test("node=4,idx=1", block_index%get_child_num(4, 1), 1)
    call end_tests()

  end subroutine


  
end program block_index_test