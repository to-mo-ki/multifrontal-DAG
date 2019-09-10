module symbolic_factorize_m
  use jagged_array_m
  implicit none
  private

  public :: symbolic_factorize
  
contains
  type(jagged_array_c) function symbolic_factorize(ccs_a, first_node, cc, tree_child, order) result(ccs_l)
    use sort_m
    type(jagged_array_c), intent(in) :: ccs_a
    integer, pointer, contiguous, intent(in) :: first_node(:), cc(:)
    type(jagged_array_c), intent(in) :: tree_child
    integer, intent(in) :: order
    integer, pointer, contiguous :: col(:), row(:)
    integer :: i, j, k, n, num_vals, child, row_num, ptr
    integer, allocatable :: full_array(:)
    integer, pointer, contiguous :: childs(:), rows_a(:), rows_l(:), rows_child(:)

    n = ccs_a%get_num_arrays()
    allocate(col(n+1))
    col(1) = 1
    do i=1, n
      col(i+1) = col(i) + cc(i)
    enddo
    num_vals = col(n+1)-1
    allocate(row(num_vals))
    
    ccs_l = create_jagged_array(col, row)
    
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
          if(row_num < first_node(i+1) .or. full_array(row_num) == i)then
            cycle
          endif
          ptr = ptr + 1
          rows_l(ptr) = row_num
          full_array(row_num) = i
        enddo
      enddo
      call sort(rows_l, size(rows_l))
    enddo

  end function

end module