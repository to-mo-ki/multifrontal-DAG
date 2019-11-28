module sparse_matrix_maker_m
  implicit none
  private

  public :: make_ccs, make_crs, make_ccs_postordering, make_supernodal_ccs
  
contains
  subroutine make_ccs(num_row, row)
    ! [ 1                 ]
    ! [   2               ]
    ! [     3             ]
    ! [   *   4           ]
    ! [     *   5         ]
    ! [   *     * 6       ]
    ! [ *           7     ]
    ! [ *   *   *     8   ]
    ! [   *   *   * *   9 ]
    integer, pointer, contiguous :: num_row(:), row(:)
    integer :: n, nonzero

    n = 9
    nonzero = 21

    allocate(num_row(n), row(nonzero))
    num_row = [3, 4, 3, 2, 3, 2, 2, 1, 1]
    row = [1, 7, 8, 2, 4, 6, 9, 3, 5, 8, 4, 9, 5, 6, 8, 6, 9, 7, 9, 8, 9]

  end subroutine

  subroutine make_crs(num_col, col)
    ! [ 1                 ]
    ! [   2               ]
    ! [     3             ]
    ! [   *   4           ]
    ! [     *   5         ]
    ! [   *     * 6       ]
    ! [ *           7     ]
    ! [ *   *   *     8   ]
    ! [   *   *   * *   9 ]
    integer, pointer, contiguous :: col(:), num_col(:)
    integer :: n, nonzero

    n = 9
    nonzero = 21

    allocate(num_col(n), col(nonzero))
    num_col = [1, 1, 1, 2, 2, 3, 2, 4, 5]
    col = [1, 2, 3, 2, 4, 3, 5, 2, 5, 6, 1, 7, 1, 3, 5, 8, 2, 4, 6, 7, 9]

  end subroutine

  subroutine make_ccs_postordering(num_row, row)
    ! [ 1                 ]
    ! [   2               ]
    ! [     3             ]
    ! [   *   4           ]
    ! [     *   5         ]
    ! [   *     * 6       ]
    ! [ *           7     ]
    ! [ *   *   *     8   ]
    ! [   *   *   * *   9 ]
    integer, pointer, contiguous :: num_row(:), row(:)
    integer :: n, nonzero

    n = 9
    nonzero = 21

    allocate(num_row(n), row(nonzero))
    num_row = [3, 2, 4, 2, 3, 3, 2, 1, 1]
    row = [1, 2, 8, 2, 9, 3, 4, 7, 9, 4, 9, 5, 6, 8, 6, 7, 8, 7, 9, 8, 9]

  end subroutine

  subroutine make_supernodal_ccs(num_row, row, node_size)
    ! [ 1                 ]
    ! [   2               ]
    ! [     3             ]
    ! [   *   4           ]
    ! [     *   5         ]
    ! [   *     * 6       ]
    ! [ *           7     ]
    ! [ *   *   *     8   ]
    ! [   *   *   * *   9 ]
    integer, pointer, contiguous, optional :: num_row(:), row(:), node_size(:)

    if(present(num_row))then
      allocate(num_row(7))
      num_row = [2, 2, 2, 2, 2, 2, 0]
    endif
    if(present(row))then
      allocate(row(12))
      row = [2, 8, 8, 9, 7, 9, 6, 8, 7, 8, 8, 9]
    endif
    if(present(node_size))then
      allocate(node_size(7))
      node_size = [1, 1, 2, 1, 1, 1, 2]
    endif
  end subroutine

end module