program factors_test
  use contiguous_sets_m
  use jagged_array_m
  use factors_m
  use test_util
  use partial_sum_m
  implicit none

  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: ccs
  integer :: nb, i, num_node

  nb = 3
  num_node = 3
  node_sets => create_contiguous_sets((/1, 6, 4/))
  ccs => create_jagged_array((/4, 5, 0/))
  factors => create_factors(node_sets, ccs, nb)

  call check_num_block(1, 2)
  call check_num_block(2, 4)
  call check_num_block(3, 2)

  call check_work_start_index(1, 1)
  call check_work_start_index(2, 3)
  
  call check_exist_border(1, .true.)
  call check_exist_border(2, .false.)
  call check_exist_border(3, .true.)

  call check_matrix_size(1)
  call check_matrix_size(2)
  call check_matrix_size(3)

  do i=1, 3
    call set_zero(i)
    call set_mark(i)
    call check_mark(i)
  enddo

  nb = 3
  num_node = 6
  node_sets => create_contiguous_sets((/5, 6, 7, 5, 3, 6/))
  ccs => create_jagged_array((/5, 4, 4, 4, 6, 0/))
  factors => create_factors(node_sets, ccs, nb)

  call check_num_block(1, 4)
  call check_num_block(2, 4)
  call check_num_block(3, 4)
  call check_num_block(4, 3)
  call check_num_block(5, 3)
  call check_num_block(6, 2)

  call check_work_start_index(1, 2)
  call check_work_start_index(2, 3)
  call check_work_start_index(3, 3)
  call check_work_start_index(4, 2)
  call check_work_start_index(5, 2)
  
  call check_exist_border(1, .true.)
  call check_exist_border(2, .false.)
  call check_exist_border(3, .true.)
  call check_exist_border(4, .true.)
  call check_exist_border(5, .false.)
  call check_exist_border(6, .false.)


  do i=1, num_node
    call check_matrix_size(i)
  enddo

  do i=1, num_node
    call set_zero(i)
    call set_mark(i)
    call check_mark(i)
  enddo  


contains

  subroutine check_num_block(node, check)
    integer, intent(in) :: node, check
    call assert_equal("num_block:node="//trim(to_str(node)), factors%get_num_block(node), check)
  end subroutine

  subroutine check_work_start_index(node, check)
    integer, intent(in) :: node, check
    call assert_equal("work_start_index:node="//trim(to_str(node)), factors%get_work_start_index(node), check)
  end subroutine

  subroutine check_exist_border(node, check)
    integer, intent(in) :: node
    logical, intent(in) :: check
    call assert_equal("exist_border:node="//trim(to_str(node)), factors%exist_border(node), check)
  end subroutine

  subroutine check_matrix_size(node)
    integer, intent(in) :: node
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, block_size, num_block, n
    
    n = ccs%get_array_length(node) + node_sets%get_length(node)
    do j=1, factors%get_num_block(node)
      do i=j, factors%get_num_block(node)
        matrix => factors%get_matrix_ptr(node, i, j)
        block_size = get_block_size(i, nb, n)*get_block_size(j, nb, n)
        call assert_equal("matrix size:node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), size(matrix), block_size)
      enddo
    enddo
  end subroutine

  subroutine set_zero(node)
    integer, intent(in) :: node
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, num_block, work_start_index
    
    num_block = factors%get_num_block(node)
    work_start_index = factors%get_work_start_index(node)

    if(factors%exist_border(node))then
      do j=1, work_start_index
        do i=j, num_block
          matrix => factors%get_supernode_ptr(node, i, j)
          matrix = 0.0d0
        enddo
      enddo
      do i=work_start_index, num_block
        matrix => factors%get_border_ptr(node, i, work_start_index)
        matrix = 0.0d0
      enddo
      if(node /= num_node)then
        do j=work_start_index, num_block
          do i=j, num_block
            matrix => factors%get_work_ptr(node, i, j)
            matrix = 0.0d0
          enddo
        enddo
      endif
    else
      do j=1, work_start_index-1
        do i=j, num_block
          matrix => factors%get_supernode_ptr(node, i, j)
          matrix = 0.0d0
        enddo
      enddo
      if(node /= num_node)then
        do j=work_start_index, num_block
          do i=j, num_block
            matrix => factors%get_work_ptr(node, i, j)
            matrix = 0.0d0
          enddo
        enddo
      endif
    endif
    
  end subroutine

  subroutine set_mark(node)
    integer, intent(in) :: node
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, block_size, num_block
    
    num_block = factors%get_num_block(node)
    do j=1, num_block
      do i=j, num_block
        matrix => factors%get_matrix_ptr(node, i, j)
        matrix = matrix + 1d0
      enddo
    enddo

  end subroutine

  subroutine check_mark(node)
    integer, intent(in) :: node
    double precision, pointer, contiguous :: matrix(:)
    integer :: i, j, num_block, work_start_index, nc, nr, block_size, fw
    
    num_block = factors%get_num_block(node)
    work_start_index = factors%get_work_start_index(node)
    nc = node_sets%get_length(node)
    nr = ccs%get_array_length(node)
    if(factors%exist_border(node))then
      do j=1, work_start_index-1
        do i=j, num_block
          matrix => factors%get_supernode_ptr(node, i, j)
          block_size = get_block_size(i, nb, nc+nr)*get_block_size(j, nb, nc)
          call assert_equal("supernode in not border:node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), matrix, (/(1d0, i=1, block_size)/))
        enddo
      enddo
      do i=work_start_index, num_block
        matrix => factors%get_border_ptr(node, i, work_start_index)
        block_size = get_block_size(i, nb, nc+nr)*get_block_size(work_start_index, nb, nc+nr)
        call assert_equal("border:node="//trim(to_str(node))//", i="//trim(to_str(i)), matrix, (/(1d0, i=1, block_size)/))
      enddo
      do j=work_start_index+1, num_block
        do i=j, num_block
          matrix => factors%get_work_ptr(node, i, j)
          block_size = get_block_size(i, nb, nc+nr)*get_block_size(j, nb, nc+nr)
          call assert_equal("work in not border:node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), matrix, (/(1d0, i=1, block_size)/))
        enddo
      enddo
      do i=work_start_index, num_block
        matrix => factors%get_supernode_ptr(node, i, work_start_index)
        block_size = get_block_size(i, nb, nc+nr)*get_block_size(work_start_index, nb, nc)
        call assert_equal("supernode in border:node="//trim(to_str(node))//", i="//trim(to_str(i)), matrix, (/(0d0, i=1, block_size)/))
      enddo
      if(node /= num_node)then
        do i=work_start_index, num_block
          matrix => factors%get_work_ptr(node, i, work_start_index)
          fw = nb - mod(nc, nb)
          block_size = get_block_size2(i-nc/nb, fw, nb, nr)*get_block_size2(work_start_index-nc/nb, fw, nb, nr)
          call assert_equal("work in not border:node="//trim(to_str(node))//", i="//trim(to_str(i)), matrix, (/(0d0, i=1, block_size)/))
        enddo
      endif
    else
      do j=1, work_start_index-1
        do i=j, num_block
          matrix => factors%get_supernode_ptr(node, i, j)
          block_size = get_block_size(i, nb, nc+nr)*get_block_size(j, nb, nc)
          call assert_equal("supernode:node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), matrix, (/(1d0, i=1, block_size)/))
        enddo
      enddo
      if(node /= num_node)then
        do j=work_start_index, num_block
          do i=j, num_block
            matrix => factors%get_work_ptr(node, i, j)
            block_size = get_block_size(i, nb, nc+nr)*get_block_size(j, nb, nc+nr)
            call assert_equal("work:node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), matrix, (/(1d0, i=1, block_size)/))
          enddo
        enddo
      endif
    endif
    
  end subroutine

end program factors_test