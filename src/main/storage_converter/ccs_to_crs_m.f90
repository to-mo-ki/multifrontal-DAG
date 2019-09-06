module ccs_to_crs_m
  use jagged_array_m
  implicit none
  private

  public :: ccs_to_crs
  
contains
  subroutine ccs_to_crs(ccs, crs)
    type(jagged_array_c), intent(in) :: ccs
    type(jagged_array_c), intent(out) :: crs
    integer :: i, j, n, nonzero, offset
    integer, pointer, contiguous :: crs_row(:), crs_col(:), rows(:)
    integer, allocatable :: num_rows(:), row_ptr(:)
    n = ccs%get_num_arrays()
    nonzero = ccs%get_num_vals()
    allocate(num_rows(n))
    allocate(row_ptr(n))
    allocate(crs_row(n+1), crs_col(nonzero))

    num_rows = 0
    do j=1,n
      rows => ccs%get_array(j)
      do i=1, size(rows)
        num_rows(rows(i)) = num_rows(rows(i)) + 1
      enddo
    enddo

    crs_row(1) = 1
    do i=1, n
      crs_row(i+1) = crs_row(i) + num_rows(i)
    enddo

    row_ptr = crs_row
    do j=1, n
      rows => ccs%get_array(j)
      do i=1, size(rows)
        crs_col(row_ptr(rows(i))) = j
        row_ptr(rows(i)) = row_ptr(rows(i)) + 1
      enddo
    enddo

    crs = create_jagged_array(crs_row, crs_col)
    
  end subroutine
end module