module reordering_m
  use contiguous_sets_m
  use jagged_array_m
  implicit none
  private

  public :: reordering_tree, reordering_ccs, reordering_ccs_val
  
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

  function reordering_ccs(ccs_origin, perm, iperm) result(ccs_reordered)
    ! NOTE: ccsのrowの順番は変わらない
    type(jagged_array_c), pointer :: ccs_reordered
    type(jagged_array_c), pointer, intent(in) :: ccs_origin
    integer, pointer, contiguous, intent(in) :: perm(:), iperm(:)
    integer, pointer, contiguous :: rows_origin(:), rows_reordered(:)
    integer, allocatable :: num_col(:)
    integer :: n, i, col_origin, col_reordered

    n = ccs_origin%get_num_arrays()
    allocate(num_col(n))

    do col_reordered=1,n
      col_origin = perm(col_reordered)
      num_col(col_reordered) = ccs_origin%get_array_length(col_origin)
    enddo

    ccs_reordered => create_jagged_array(num_col)

    do col_origin=1, n
      col_reordered = iperm(col_origin)
      rows_origin => ccs_origin%get_array(col_origin)
      rows_reordered => ccs_reordered%get_array(col_reordered)
      do i=1, size(rows_reordered)
        rows_reordered(i) = iperm(rows_origin(i))
      end do
    end do

  end function

  function reordering_ccs_val(origin_ccs_set, reorder_ccs_set, origin_val, perm) result(reorder_val)
    type(contiguous_sets_c), pointer :: origin_ccs_set, reorder_ccs_set
    double precision, contiguous, target :: origin_val(:)
    double precision, pointer, contiguous :: reorder_val(:)
    double precision, pointer, contiguous :: reorder_val_ptr(:), origin_val_ptr(:)
    integer, contiguous :: perm(:)
    integer :: reorder_idx, origin_idx

    allocate(reorder_val(size(origin_val)))

    do reorder_idx=1,origin_ccs_set%get_num_sets()
      origin_idx = perm(reorder_idx)
      reorder_val_ptr => reorder_val(reorder_ccs_set%get_first(reorder_idx):reorder_ccs_set%get_last(reorder_idx))
      origin_val_ptr => origin_val(origin_ccs_set%get_first(origin_idx):origin_ccs_set%get_last(origin_idx))
      reorder_val_ptr = origin_val_ptr
    enddo

  end function


end module