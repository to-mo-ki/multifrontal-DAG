module sym_to_asym_m
  use jagged_array_m
  implicit none
  private

  public :: sym_to_asym
  
contains
  subroutine sym_to_asym(sym, asym)
    ! NOTE: Not include diagonal, CCS
    type(jagged_array_c), intent(in) :: sym
    type(jagged_array_c), intent(out) :: asym
    integer, pointer, contiguous :: asym_row(:), asym_col(:), rows(:)
    integer :: ptr, i, j, n, sym_nonzero, asym_nonzero, offset, col
    integer, allocatable :: num_rows(:), col_ptr(:)
    
    n = sym%get_num_arrays()
    sym_nonzero = sym%get_num_vals()

    allocate(num_rows(n), col_ptr(n))
    asym_nonzero = (sym_nonzero-n)*2
    allocate(asym_col(n+1), asym_row(asym_nonzero))
    num_rows = 0
    !Lにおけるそれぞれの行の数をカウント（対角部分込みで）
    do j=1, n
      rows => sym%get_array(j)
      do i=1, size(rows)
       num_rows(rows(i)) = num_rows(rows(i)) + 1 
      enddo
    enddo

    !colを作成
    asym_col(1) = 1
    do i=1, n
      ! NOTE: -2 is diagonal
      asym_col(i+1) = asym_col(i) + sym%get_array_length(i) + num_rows(i) - 2
    enddo
    !下半分を格納
    do j=1, n
      offset = asym_col(j) + num_rows(j) - 3
      rows => sym%get_array(j)
      do i=2, size(rows)
        asym_row(offset + i) = rows(i)
      enddo
    enddo

    !上半分を格納
    col_ptr = asym_col(:n)
    do j=1, n
      rows => sym%get_array(j)
      do i=2, size(rows)
        asym_row(col_ptr(rows(i))) = j
        col_ptr(rows(i)) = col_ptr(rows(i)) + 1
      enddo
    enddo

    asym = create_jagged_array(asym_col, asym_row)

  end subroutine
end module