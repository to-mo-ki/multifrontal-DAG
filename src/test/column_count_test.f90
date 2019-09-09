program column_count_test
  use jagged_array_m
  use column_count_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs, tree_child
  integer, pointer, contiguous :: col(:), row(:), cc(:), child_ptr(:), child_val(:), parent(:)
  integer :: i

  call make_ccs_postordering(col, row)
  ccs = create_jagged_array(col, row)

  allocate(parent(9))
  parent = (/2, 8, 4, 7, 6, 7, 8, 9, 0/)
  
  allocate(child_ptr(10), child_val(9))
  child_ptr = (/1, 1, 2, 2, 3, 3, 4, 6, 8, 9/)
  child_val = (/1, 3, 5, 4, 6, 2, 7, 8, 0/)
  tree_child = create_jagged_array(child_ptr, child_val)
  
  cc => column_count(ccs, tree_child, parent)

  call assert_equal("column count", cc, (/3, 3, 4, 3, 3, 3, 3, 2, 1/))

  
end program column_count_test