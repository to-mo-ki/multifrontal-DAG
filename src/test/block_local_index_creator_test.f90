program block_local_index_creator_test
  use contiguous_sets_m
  use jagged_array_m
  use jagged_array_3D_m
  use block_local_index_creator_m
  use node_data_m
  use test_util
  implicit none
  type(jagged_array_c), pointer :: local_index
  type(node_data_c), pointer :: node_data
  type(jagged_array_c), pointer :: parent_block_num, parent_block_ptr
  type(contiguous_sets_c), pointer :: parent_ptr
  type(jagged_array_3D_c), pointer :: block_local_index
  integer, pointer, contiguous :: local_index_val(:)

  call test1
  call test2
  call test3
  
contains
  subroutine test1()
    local_index => create_jagged_array((/4, 0/), (/2, 3, 7, 8/))
    
    parent_ptr => create_parent_ptr(local_index, 3)
    call assert_equal("parent_ptr:1", parent_ptr%get_length(1), 2)

    parent_block_num => create_parent_block_num(local_index, parent_ptr, 3)
    call assert_equal("parent_block_num", parent_block_num%get_array(1), (/1, 3/))

    parent_block_ptr => create_parent_block_ptr(local_index, parent_ptr, 3)
    call assert_equal("parent_block_ptr", parent_block_ptr%get_array(1), (/1, 3/))

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 3)
    call assert_equal("rebuild_val", local_index_val, (/2, 3, 1, 2/))

  end subroutine

  subroutine test2()
    local_index => create_jagged_array((/3, 2, 2, 1, 0/), (/2, 4, 6, 1, 5, 2, 3, 1/))

    parent_ptr => create_parent_ptr(local_index, 2)
    call assert_equal("parent_ptr:1", parent_ptr%get_length(1), 3)
    call assert_equal("parent_ptr:2", parent_ptr%get_length(2), 2)
    call assert_equal("parent_ptr:3", parent_ptr%get_length(3), 2)
    call assert_equal("parent_ptr:4", parent_ptr%get_length(4), 1)

    parent_block_num => create_parent_block_num(local_index, parent_ptr, 2)
    call assert_equal("parent_block_num:1", parent_block_num%get_array(1), (/1, 2, 3/))
    call assert_equal("parent_block_num:2", parent_block_num%get_array(2), (/1, 3/))
    call assert_equal("parent_block_num:3", parent_block_num%get_array(3), (/1, 2/))
    call assert_equal("parent_block_num:4", parent_block_num%get_array(4), (/1/))

    parent_block_ptr => create_parent_block_ptr(local_index, parent_ptr, 2)
    call assert_equal("parent_block_ptr:1", parent_block_ptr%get_array(1), (/1, 2, 3/))
    call assert_equal("parent_block_ptr:2", parent_block_ptr%get_array(2), (/4, 5/))
    call assert_equal("parent_block_ptr:3", parent_block_ptr%get_array(3), (/6, 7/))
    call assert_equal("parent_block_ptr:4", parent_block_ptr%get_array(4), (/8/))

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 2, 1, 1, 2, 1, 1/))

  end subroutine

  subroutine test3()
    local_index => create_jagged_array((/3, 2, 3, 1, 0/), (/2, 4, 7, 1, 6, 1, 2, 3, 1/))

    parent_ptr => create_parent_ptr(local_index, 2)
    call assert_equal("parent_ptr:1", parent_ptr%get_length(1), 3)
    call assert_equal("parent_ptr:2", parent_ptr%get_length(2), 2)
    call assert_equal("parent_ptr:3", parent_ptr%get_length(3), 2)
    call assert_equal("parent_ptr:4", parent_ptr%get_length(4), 1)

    parent_block_num => create_parent_block_num(local_index, parent_ptr, 2)
    call assert_equal("parent_block_num:1", parent_block_num%get_array(1), (/1, 2, 4/))
    call assert_equal("parent_block_num:2", parent_block_num%get_array(2), (/1, 3/))
    call assert_equal("parent_block_num:3", parent_block_num%get_array(3), (/1, 2/))
    call assert_equal("parent_block_num:4", parent_block_num%get_array(4), (/1/))

    parent_block_ptr => create_parent_block_ptr(local_index, parent_ptr, 2)
    call assert_equal("parent_block_ptr:1", parent_block_ptr%get_array(1), (/1, 2, 3/))
    call assert_equal("parent_block_ptr:2", parent_block_ptr%get_array(2), (/4, 5/))
    call assert_equal("parent_block_ptr:3", parent_block_ptr%get_array(3), (/6, 8/))
    call assert_equal("parent_block_ptr:4", parent_block_ptr%get_array(4), (/9/))

    local_index_val => local_index%get_raw_val()
    call rebuild_val(local_index_val, 2)
    call assert_equal("rebuild_val", local_index_val, (/2, 2, 1, 1, 2, 1, 2, 1, 1/))

  end subroutine

  

end program block_local_index_creator_test