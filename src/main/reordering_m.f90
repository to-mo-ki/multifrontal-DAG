module reordering_m
  use jagged_array_m
  implicit none
  private

  public :: reordering_tree, reordering_ccs
  
contains
  function reordering_tree(parent_origin, perm, iperm) result(parent_reordered)
    integer, pointer, contiguous :: parent_reordered(:)
    integer, pointer, contiguous, intent(in) :: parent_origin(:), perm(:), iperm(:)
    integer :: n, i

    n = size(parent_origin)
    allocate(parent_reordered(n))
    do i=1, n-1
      parent_reordered(i) = iperm(parent_origin(perm(i)))
    enddo
    parent_reordered(n) = 0

  end function

  type(jagged_array_c) function reordering_ccs(ccs_origin, perm, iperm) result(ccs_reordered)
    ! NOTE: ccsのrowの順番は変わらない
    type(jagged_array_c), intent(in) :: ccs_origin
    integer, pointer, contiguous, intent(in) :: perm(:), iperm(:)
    integer, pointer, contiguous :: ccs_col(:), ccs_row(:), rows_origin(:), rows_reordered(:)
    integer, allocatable :: num_cols(:)
    integer :: n, nonzero, i, col_origin, col_reordered

    n = ccs_origin%get_num_arrays()
    nonzero = ccs_origin%get_num_vals()
    allocate(ccs_col(n+1), ccs_row(nonzero))

    ccs_col(1) = 1
    do col_reordered=1,n
      col_origin = perm(col_reordered)
      ccs_col(col_reordered+1) = ccs_col(col_reordered) + ccs_origin%get_array_length(col_origin)
    enddo
    
    do col_origin=1, n
      col_reordered = iperm(col_origin)
      rows_origin => ccs_origin%get_array(col_origin)
      rows_reordered => ccs_row(ccs_col(col_reordered):ccs_col(col_reordered+1)-1)
      do i=1, size(rows_reordered)
        rows_reordered(i) = iperm(rows_origin(i))
      end do
    end do

    ccs_reordered = create_jagged_array(ccs_col, ccs_row)

  end function
end module