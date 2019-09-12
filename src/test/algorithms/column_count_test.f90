program column_count_test
  use jagged_array_m
  use column_count_m
  use sparse_matrix_maker_m
  use test_util
  implicit none

  type(jagged_array_c) :: ccs, tree_child
  integer, pointer, contiguous :: num_row(:), row(:), cc(:), num_child(:), child_val(:), parent(:)
  integer :: i

  call make_ccs_postordering(num_row, row)
  ccs = create_jagged_array(num_row, row)

  allocate(parent(9))
  parent = (/2, 8, 4, 7, 6, 7, 8, 9, 0/)
  
  allocate(num_child(9), child_val(8))
  num_child = (/0, 1, 0, 1, 0, 1, 2, 2, 1/)
  child_val = (/1, 3, 5, 4, 6, 2, 7, 8/)
  tree_child = create_jagged_array(num_child, child_val)
  
  cc => column_count(ccs, tree_child, parent)

  call assert_equal("column count", cc, (/3, 3, 4, 3, 3, 3, 3, 2, 1/))

  
end program column_count_test