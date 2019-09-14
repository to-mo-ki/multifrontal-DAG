module sym_to_asym_m
  use jagged_array_m
  implicit none
  private

  public :: sym_to_asym
  
contains
  subroutine sym_to_asym(sym, asym)
    ! NOTE: Not include diagonal, CCS
    ! HACK: 特に変数名
    type(jagged_array_c), pointer, intent(in) :: sym
    type(jagged_array_c), pointer, intent(out) :: asym
    integer, pointer, contiguous :: asym_rows(:), sym_rows(:)
    integer :: ptr, i, j, n, sym_nonzero, asym_nonzero, offset, col
    integer, allocatable :: num_row(:), col_ptr(:), num_col(:)
    
    n = sym%get_num_arrays()
    sym_nonzero = sym%get_num_vals()

    allocate(num_row(n), col_ptr(n))
    asym_nonzero = (sym_nonzero-n)*2
    
    num_row = 0
    !Lにおけるそれぞれの行の数をカウント（対角部分込みで）
    do j=1, n
      sym_rows => sym%get_array(j)
      do i=1, size(sym_rows)
       num_row(sym_rows(i)) = num_row(sym_rows(i)) + 1 
      enddo
    enddo

    allocate(num_col(n))
    !colを作成
    do i=1, n
      ! NOTE: -2 is diagonal
      num_col(i) = sym%get_array_length(i) + num_row(i) - 2
    enddo

    asym => create_jagged_array(num_col)

    !下半分を格納
    do j=1, n
      offset = num_row(j) - 2
      sym_rows => sym%get_array(j)
      asym_rows => asym%get_array(j)
      do i=2, size(sym_rows)
        asym_rows(offset + i) = sym_rows(i)
      enddo
    enddo

    !上半分を格納
    col_ptr = 1
    do j=1, n
      sym_rows => sym%get_array(j)
      do i=2, size(sym_rows)
        asym_rows => asym%get_array(sym_rows(i))
        asym_rows(col_ptr(sym_rows(i))) = j
        col_ptr(sym_rows(i)) = col_ptr(sym_rows(i)) + 1
      enddo
    enddo

  end subroutine
end module