program block_local_index_test
  use jagged_array_m
  use contiguous_sets_m
  use block_local_index_m
  use test_util
  implicit none
  type(contiguous_sets_c), pointer :: node_set
  type(jagged_array_c), pointer :: local_index
  integer :: nb
  type(block_local_index_c), pointer :: block_local_index

  call test1
  call test2
  call test3
  call test4

contains
  subroutine test1()
    local_index => create_jagged_array((/4, 0/), (/2, 3, 7, 8/))
    node_set => create_contiguous_sets((/5, 8/))
    nb = 3
    block_local_index => create_block_local_index(node_set, local_index, nb)
    
    call assert_equal("block_nums:node=1", block_local_index%get_block_nums(1), [1, 3])
    call assert_equal("over:node=1", block_local_index%get_overs(1), [1, 0]) 
    call assert_equal("local_index:node=1, block_num=1", block_local_index%get_local_index(1, 1), [2, 3])
    call assert_equal("local_index:node=1, block_num=2", block_local_index%get_local_index(1, 2), [1, 2])
    
  end subroutine

  subroutine test2()
    local_index => create_jagged_array((/3, 2, 2, 1, 0/), (/2, 4, 6, 1, 5, 2, 3, 1/))
    node_set => create_contiguous_sets((/3, 2, 4, 1, 3/))
    nb = 2
    block_local_index => create_block_local_index(node_set, local_index, nb)
    
    call assert_equal("block_nums:node=1", block_local_index%get_block_nums(1), [1, 2, 3])
    call assert_equal("block_nums:node=2", block_local_index%get_block_nums(2), [1, 3])
    call assert_equal("block_nums:node=3", block_local_index%get_block_nums(3), [1, 2])
    call assert_equal("block_nums:node=4", block_local_index%get_block_nums(4), [1])
    call assert_equal("over:node=1", block_local_index%get_overs(1), [0, -1, 0]) 
    call assert_equal("over:node=2", block_local_index%get_overs(2), [-1, 0]) 
    call assert_equal("over:node=3", block_local_index%get_overs(3), [-1, 0]) 
    call assert_equal("over:node=4", block_local_index%get_overs(4), [0]) 
    call assert_equal("local_index:node=1, block_num=1", block_local_index%get_local_index(1, 1), [2])
    call assert_equal("local_index:node=1, block_num=2", block_local_index%get_local_index(1, 2), [2])
    call assert_equal("local_index:node=1, block_num=3", block_local_index%get_local_index(1, 3), [2])
    call assert_equal("local_index:node=2, block_num=1", block_local_index%get_local_index(2, 1), [1])
    call assert_equal("local_index:node=2, block_num=2", block_local_index%get_local_index(2, 2), [1])
    call assert_equal("local_index:node=3, block_num=1", block_local_index%get_local_index(3, 1), [2])
    call assert_equal("local_index:node=3, block_num=2", block_local_index%get_local_index(3, 2), [1])
    call assert_equal("local_index:node=4, block_num=1", block_local_index%get_local_index(4, 1), [1])
    
  end subroutine


  subroutine test3()
    local_index => create_jagged_array((/3, 2, 3, 1, 0/), (/2, 4, 7, 1, 6, 1, 2, 3, 1/))
    node_set => create_contiguous_sets((/3, 2, 4, 1, 3/))
    nb = 2
    block_local_index => create_block_local_index(node_set, local_index, nb)
    
    call assert_equal("block_nums:node=1", block_local_index%get_block_nums(1), [1, 2, 4])
    call assert_equal("block_nums:node=2", block_local_index%get_block_nums(2), [1, 3])
    call assert_equal("block_nums:node=3", block_local_index%get_block_nums(3), [1, 2])
    call assert_equal("block_nums:node=4", block_local_index%get_block_nums(4), [1])
    call assert_equal("over:node=1", block_local_index%get_overs(1), [0, -1, 0]) 
    call assert_equal("over:node=2", block_local_index%get_overs(2), [-1, 0]) 
    call assert_equal("over:node=3", block_local_index%get_overs(3), [0, -1]) 
    call assert_equal("over:node=4", block_local_index%get_overs(4), [0]) 
    call assert_equal("local_index:node=1, block_num=1", block_local_index%get_local_index(1, 1), [2])
    call assert_equal("local_index:node=1, block_num=2", block_local_index%get_local_index(1, 2), [2])
    call assert_equal("local_index:node=1, block_num=3", block_local_index%get_local_index(1, 3), [1])
    call assert_equal("local_index:node=2, block_num=1", block_local_index%get_local_index(2, 1), [1])
    call assert_equal("local_index:node=2, block_num=2", block_local_index%get_local_index(2, 2), [2])
    call assert_equal("local_index:node=3, block_num=1", block_local_index%get_local_index(3, 1), [1, 2])
    call assert_equal("local_index:node=3, block_num=2", block_local_index%get_local_index(3, 2), [1])
    call assert_equal("local_index:node=4, block_num=1", block_local_index%get_local_index(4, 1), [1])
    
  end subroutine


  subroutine test4()
    local_index => create_jagged_array((/5, 2, 0/), (/2, 3, 4, 5, 7, 3, 4/))
    node_set => create_contiguous_sets((/4, 5, 4/))
    nb = 3
    block_local_index => create_block_local_index(node_set, local_index, nb)
    
    call assert_equal("block_nums:node=1", block_local_index%get_block_nums(1), [1, 2, 3])
    call assert_equal("block_nums:node=2", block_local_index%get_block_nums(2), [1, 2])
    call assert_equal("over:node=1", block_local_index%get_overs(1), [0, -1, 0])
    call assert_equal("over:node=2", block_local_index%get_overs(2), [0, -1])

    call assert_equal("local_index:node=1, block_num=1", block_local_index%get_local_index(1, 1), [2, 3])
    call assert_equal("local_index:node=1, block_num=2", block_local_index%get_local_index(1, 2), [1, 2])
    call assert_equal("local_index:node=1, block_num=3", block_local_index%get_local_index(1, 3), [1])
    call assert_equal("local_index:node=2, block_num=1", block_local_index%get_local_index(2, 1), [3])
    call assert_equal("local_index:node=2, block_num=2", block_local_index%get_local_index(2, 2), [1])
    
  end subroutine

  
end program block_local_index_test