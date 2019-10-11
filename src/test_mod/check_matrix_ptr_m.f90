module check_matrix_ptr_m
  use factors_m
  use test_util
  implicit none
  
contains
  subroutine check_matrix_ptr(num_node, factors)
    integer, intent(in) :: num_node
    type(factors_c), pointer ::  factors
    integer :: i
    do i=1, num_node
      call set_zero(i, num_node)
      call set_mark(i)
      call check_mark(i, num_node)
    enddo
  contains
    subroutine search(node, jstart, jend, iend, extract_matrix, action)
      integer,intent(in) :: node, jstart, jend, iend
      procedure(extract_matrix_i) :: extract_matrix
      procedure(action_i) :: action
      integer :: i, j
      double precision, pointer, contiguous :: matrix(:)
      interface
        function extract_matrix_i(node, i, j) result(matrix)
          double precision, pointer, contiguous :: matrix(:)
          integer, intent(in) :: node, i, j
        end function

        subroutine action_i(matrix)
          double precision, pointer, contiguous :: matrix(:)
        end subroutine
      end interface

      do j=jstart, jend
        do i=j, iend
          matrix => extract_matrix(node, i, j)
          call action(matrix)
        enddo
      enddo

    end subroutine

    subroutine a_set_zero(matrix)
      double precision, pointer, contiguous :: matrix(:)
      matrix = 0.0d0
    end subroutine

    subroutine a_set_mark(matrix)
      double precision, pointer, contiguous :: matrix(:)
      matrix = matrix + 1.0d0
    end subroutine

    subroutine a_check_mark(matrix)
      double precision, pointer, contiguous :: matrix(:)
      integer :: i
      call assert_equal("test", matrix, (/(1d0, i=1, size(matrix))/))
    end subroutine

    subroutine a_check_no_mark(matrix)
      double precision, pointer, contiguous :: matrix(:)
      integer :: i
      call assert_equal("test", matrix, (/(0d0, i=1, size(matrix))/))
    end subroutine

    function extract_supernode(node, i, j) result(matrix)
      double precision, pointer, contiguous :: matrix(:)
      integer, intent(in) :: node, i, j
      matrix => factors%get_supernode_ptr(node, i, j)
    end function

    function extract_border(node, i, j) result(matrix)
      double precision, pointer, contiguous :: matrix(:)
      integer, intent(in) :: node, i, j
      matrix => factors%get_border_ptr(node, i, j)
    end function

    function extract_work(node, i, j) result(matrix)
      double precision, pointer, contiguous :: matrix(:)
      integer, intent(in) :: node, i, j
      matrix => factors%get_work_ptr(node, i, j)
    end function

    function extract_matrix(node, i, j) result(matrix)
      double precision, pointer, contiguous :: matrix(:)
      integer, intent(in) :: node, i, j
      matrix => factors%get_matrix_ptr(node, i, j)
    end function

    subroutine set_zero(node, num_node)
      integer, intent(in) :: node, num_node
      integer :: num_block, work_start_index

      num_block = factors%get_num_block(node)
      work_start_index = factors%get_work_start_index(node)

      if(factors%exist_border(node))then
        call search(node, 1, work_start_index, num_block, extract_supernode, a_set_zero)
        call search(node, work_start_index, work_start_index, num_block, extract_border, a_set_zero)
        if(node /= num_node)then
          call search(node, work_start_index, num_block, num_block, extract_work, a_set_zero)
        endif
      else
        call search(node, 1, work_start_index-1, num_block, extract_supernode, a_set_zero)
        if(node /= num_node)then
          call search(node, work_start_index, num_block, num_block, extract_work, a_set_zero)
        endif
      endif

    end subroutine
  
    subroutine set_mark(node)
      integer, intent(in) :: node
      integer :: num_block
    
      num_block = factors%get_num_block(node)
      call search(node, 1, num_block, num_block, extract_matrix, a_set_mark)
    
    end subroutine
  
    subroutine check_mark(node, num_node)
      integer, intent(in) :: node, num_node
      integer :: num_block, work_start_index
    
      num_block = factors%get_num_block(node)
      work_start_index = factors%get_work_start_index(node)
      if(factors%exist_border(node))then
        call search(node, 1, work_start_index-1, num_block, extract_supernode, a_check_mark)
        call search(node, work_start_index, work_start_index, num_block, extract_border, a_check_mark)
        call search(node, work_start_index+1, num_block, num_block, extract_work, a_check_mark)
        call search(node, work_start_index, work_start_index, num_block, extract_supernode, a_check_no_mark)
        if(node /= num_node)then
          call search(node, work_start_index, work_start_index, num_block, extract_work, a_check_no_mark)
        endif
      else
        call search(node, 1, work_start_index-1, num_block, extract_supernode, a_check_mark)
        if(node /= num_node)then
          call search(node, work_start_index, num_block, num_block, extract_work, a_check_mark)
        endif
      endif

    end subroutine
  end subroutine
end module