module symbolic_factorize_m
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  private

  public :: symbolic_factorize
  
contains
  function symbolic_factorize(ccs_a, node_sets, cc, tree_child) result(ccs_l)
    use sort_m
    type(jagged_array_c), pointer :: ccs_l
    type(jagged_array_c), pointer, intent(in) :: ccs_a
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    integer, pointer, contiguous, intent(in) :: cc(:)
    type(jagged_array_c), pointer, intent(in) :: tree_child
    integer :: i, j, k, n, child, row_num, ptr, order, node
    integer, allocatable :: full_array(:)
    integer, pointer, contiguous :: childs(:), rows_a(:), rows_l(:), rows_child(:)

    n = ccs_a%get_num_arrays()
    order = node_sets%get_num_elements()

    ccs_l => create_jagged_array(cc)
    
    allocate(full_array(order))
    full_array = 0

    do i=1, n
      rows_l => ccs_l%get_array(i)
      ptr = 0
      rows_a => ccs_a%get_array(i)
      do k=1, size(rows_a)
        row_num = rows_a(k)
        ptr = ptr + 1
        rows_l(ptr) = row_num
        full_array(row_num) = i
      enddo
      if(tree_child%get_array_length(i) == 0) cycle
      childs => tree_child%get_array(i)
      do j=1, size(childs)
        child = childs(j)
        rows_child => ccs_l%get_array(child)
        do k=1, size(rows_child)
          row_num = rows_child(k)
          if(row_num <= node_sets%get_last(i) .or. full_array(row_num) == i)then
            cycle
          endif
          ptr = ptr + 1
          rows_l(ptr) = row_num
          full_array(row_num) = i
        enddo
      enddo
    enddo

    !TODO: TEST, node=1でソートされるような問題
    do node=1, ccs_l%get_num_arrays()
      rows_l => ccs_l%get_array(node)
      call sort(rows_l, size(rows_l))
    enddo

  end function

end module