module reordering_m
  use contiguous_sets_m
  use jagged_array_m
  use sort_m
  implicit none
  private
  
  interface repostordering_ccs
    procedure :: repostordering_ccs1
    procedure :: repostordering_ccs2
  end interface


  public :: reordering_tree, reordering_ccs, repostordering_ccs, reordering_ccs_val
  
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

  function repostordering_ccs1(ccs_origin, perm, iperm) result(ccs_reordered)
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

  function repostordering_ccs2(ccs_origin, perm, iperm, ccs_perm) result(ccs_reordered)
    type(jagged_array_c), pointer :: ccs_reordered
    type(jagged_array_c), pointer, intent(in) :: ccs_origin
    integer, pointer, contiguous, intent(in) :: perm(:), iperm(:)
    integer, pointer, contiguous, intent(out) :: ccs_perm(:)
    integer, pointer, contiguous :: rows_origin(:), rows_reordered(:), perm_ptr(:)
    integer, allocatable :: num_col(:)
    type(jagged_array_c), pointer :: perm_jag
    integer :: n, i, col_origin, col_reordered, offset

    n = ccs_origin%get_num_arrays()
    allocate(num_col(n))

    do col_reordered=1,n
      col_origin = perm(col_reordered)
      num_col(col_reordered) = ccs_origin%get_array_length(col_origin)
    enddo

    ccs_reordered => create_jagged_array(num_col)
    perm_jag => create_jagged_array(ccs_reordered%get_set())

    offset = 0
    do col_origin=1, n
      col_reordered = iperm(col_origin)
      rows_origin => ccs_origin%get_array(col_origin)
      rows_reordered => ccs_reordered%get_array(col_reordered)
      perm_ptr => perm_jag%get_array(col_reordered)
      do i=1, size(rows_reordered)
        rows_reordered(i) = iperm(rows_origin(i))
        perm_ptr(i) = offset + i
      end do
      offset = offset + size(rows_reordered)
    end do

    ccs_perm => perm_jag%get_raw_val()

  end function

  function reordering_ccs(ccs_origin, perm, iperm, ccs_perm) result(ccs_reordered)
    type(jagged_array_c), pointer :: ccs_reordered
    type(jagged_array_c), pointer, intent(in) :: ccs_origin
    integer, pointer, contiguous, intent(in) :: perm(:), iperm(:)
    integer, pointer, contiguous, intent(out) :: ccs_perm(:)
    integer, pointer, contiguous :: rows(:), reorder_rows(:), perm_ptr(:)
    type(jagged_array_c), pointer :: perm_jag
    integer, allocatable :: num_col(:), ptr(:)
    integer :: n, nonzero, i, j, col, row

    n = ccs_origin%get_num_arrays()
    nonzero = ccs_origin%get_num_vals()
    allocate(num_col(n), ptr(n))

    num_col = 0
    do j=1,n
      rows => ccs_origin%get_array(i)
      do i=1, size(rows)
        col = min(iperm(j), iperm(rows(i)))
        num_col(col) = num_col(col) + 1
      enddo
    end do

    ccs_reordered => create_jagged_array(num_col)
    
    allocate(ptr(n))
    perm_jag => create_jagged_array(ccs_reordered%get_set())

    ptr = 1
    do j=1,n
      rows => ccs_origin%get_array(j)
      do i=1,size(rows)
        col = min(iperm(j), iperm(rows(i)))
        row = max(iperm(j), iperm(rows(i)))
        reorder_rows => ccs_reordered%get_array(col)
        reorder_rows(ptr(col)) = row
        perm_ptr => perm_jag%get_array(col)
        perm_ptr(ptr(col)) = j
        ptr(col) = ptr(col) + 1
      end do
    end do

    ccs_perm => perm_jag%get_raw_val()

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