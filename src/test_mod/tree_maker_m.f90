module tree_maker_m
  implicit none
  
contains
  subroutine make_original_tree(parent, num_child, child_val)
    integer, pointer, contiguous, optional :: parent(:), num_child(:), child_val(:)
    if(present(parent))then
      allocate(parent(9))
      parent = [7, 4, 5, 6, 6, 8, 8, 9, 0]
    endif
    if(present(num_child))then
      allocate(num_child(9))
      num_child = [0, 0, 0, 1, 1, 2, 1, 2, 1]
    endif
    if(present(child_val))then
      allocate(child_val(8))
      child_val = [2, 3, 4, 5, 1, 6, 7, 8]
    endif
    
  end subroutine

  subroutine make_postordering_tree(parent, num_child, child_val)
    integer, pointer, contiguous, optional :: parent(:), num_child(:), child_val(:)
    if(present(parent))then
      allocate(parent(9))
      parent = [2, 8, 4, 7, 6, 7, 8, 9, 0]
    endif
    if(present(num_child))then
      allocate(num_child(9))
      num_child = [0, 1, 0, 1, 0, 1, 2, 2, 1]
    endif
    if(present(child_val))then
      allocate(child_val(8))
      child_val = [1, 3, 5, 4, 6, 2, 7, 8]
    endif

  end subroutine

  subroutine make_supernodal_tree(num_child, child_val)
    integer, pointer, contiguous, optional :: num_child(:), child_val(:)
    if(present(num_child))then
      allocate(num_child(7))
      num_child = [0, 1, 0, 0, 1, 2, 2]
    endif
    if(present(child_val))then
      allocate(child_val(6))
      child_val = [1, 4, 3, 5, 2, 6]
    endif
    
  end subroutine


end module