module ccs_to_crs_m
  use jagged_array_m
  implicit none
  private

  public :: ccs_to_crs
  
contains
  subroutine ccs_to_crs(ccs, crs)
    type(jagged_array_c), pointer, intent(in) :: ccs
    type(jagged_array_c), pointer, intent(out) :: crs
    integer :: i, j, n, nonzero, offset
    integer, pointer, contiguous :: crs_col(:), rows(:)
    integer, allocatable :: num_row(:), row_ptr(:)
    n = ccs%get_num_arrays()
    nonzero = ccs%get_num_vals()
    allocate(num_row(n))
    allocate(row_ptr(n))
    allocate(crs_col(nonzero))

    num_row = 0
    do j=1,n
      rows => ccs%get_array(j)
      do i=1, size(rows)
        num_row(rows(i)) = num_row(rows(i)) + 1
      enddo
    enddo

    crs => create_jagged_array(num_row, crs_col)

    row_ptr = 1
    do j=1, n
      rows => ccs%get_array(j)
      do i=1, size(rows)
        crs_col => crs%get_array(rows(i))
        crs_col(row_ptr(rows(i))) = j
        row_ptr(rows(i)) = row_ptr(rows(i)) + 1
      enddo
    enddo

    
    
  end subroutine
end module