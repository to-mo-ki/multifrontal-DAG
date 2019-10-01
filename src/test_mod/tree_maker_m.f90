module tree_maker_m
  implicit none
  
contains
  subroutine make_original_tree(parent)
    integer, pointer, contiguous, intent(out) :: parent(:)
    allocate(parent(9))
    parent = (/7, 4, 5, 6, 6, 8, 8, 9, 0/)
    
  end subroutine

  subroutine make_postordering_tree(num_child, child_val)
    integer, pointer, contiguous, intent(out) :: num_child(:)
    integer, pointer, contiguous, optional :: child_val(:)
    allocate(num_child(9))
    num_child = (/0, 1, 0, 1, 0, 1, 2, 2, 1/)
    if(present(child_val))then
      allocate(child_val(8))
      child_val = (/1, 3, 5, 4, 6, 2, 7, 8/)
    endif

  end subroutine
end module