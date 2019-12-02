module check_matrix_ptr_m
  use factors_m
  use action_m
  use extract_matrix_m
  implicit none
  
contains
  subroutine check_matrix_ptr(factors)
    type(factors_c), pointer ::  factors
    integer :: i

    do i=1, factors%get_num_node()
      call set_zero(factors, i)
      call set_mark(factors, i)
      call check_mark(factors, i)
    enddo

  end subroutine

  subroutine search(factors, node, jstart, jend, iend, extract_matrix, action)
    type(factors_c), pointer :: factors
    integer,intent(in) :: node, jstart, jend, iend
    procedure(extract_matrix_i) :: extract_matrix
    procedure(action_i) :: action
    integer :: i, j
    double precision, pointer, contiguous :: matrix(:)

    do j=jstart, jend
      do i=j, iend
        matrix => extract_matrix(factors, node, i, j)
        call action(matrix, node, i, j)
      enddo
    enddo

  end subroutine

  subroutine set_zero(factors, node)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node
    integer :: num_block, work_start_index, num_node

    num_block = factors%get_num_block(node)
    work_start_index = factors%get_work_start_index(node)
    num_node = factors%get_num_node()
    if(factors%exist_border(node))then
      call search(factors, node, 1, work_start_index, num_block, extract_supernode, a_set_zero)
      call search(factors, node, work_start_index, work_start_index, num_block, extract_border, a_set_zero)
      if(node /= num_node)then
        call search(factors, node, work_start_index, num_block, num_block, extract_work, a_set_zero)
      endif
    else
      call search(factors, node, 1, work_start_index-1, num_block, extract_supernode, a_set_zero)
      if(node /= num_node)then
        call search(factors, node, work_start_index, num_block, num_block, extract_work, a_set_zero)
      endif
    endif

  end subroutine

  subroutine set_mark(factors, node)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node
    integer :: num_block
  
    num_block = factors%get_num_block(node)
    call search(factors, node, 1, num_block, num_block, extract_matrix, a_set_mark)
  
  end subroutine

  subroutine check_mark(factors, node)
    type(factors_c), pointer :: factors
    integer, intent(in) :: node
    integer :: num_block, work_start_index, num_node
  
    num_block = factors%get_num_block(node)
    work_start_index = factors%get_work_start_index(node)
    num_node = factors%get_num_node()
    if(factors%exist_border(node))then
      print *, "check supernode"
      if(node /= num_node)then
        call search(factors, node, 1, work_start_index-1, num_block, extract_supernode, a_check_mark)
      else
        call search(factors, node, 1, work_start_index, num_block, extract_supernode, a_check_mark)
      endif
      call search(factors, node, work_start_index, work_start_index, num_block, extract_supernode, a_check_no_mark)
      print *, "check border"
      call search(factors, node, work_start_index, work_start_index, num_block, extract_border, a_check_mark)
      if(node /= num_node)then
        print *, "check work"
        call search(factors, node, work_start_index+1, num_block, num_block, extract_work, a_check_mark)
        call search(factors, node, work_start_index, work_start_index, num_block, extract_work, a_check_no_mark)
      endif
    else
      print *, "check supernode"
      call search(factors, node, 1, work_start_index-1, num_block, extract_supernode, a_check_mark)
      if(node /= num_node)then
        print *, "check work"
        call search(factors, node, work_start_index, num_block, num_block, extract_work, a_check_mark)
      endif
    endif

  end subroutine
end module