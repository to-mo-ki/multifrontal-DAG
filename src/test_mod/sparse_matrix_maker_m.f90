module sparse_matrix_maker_m
  implicit none
  private

  public :: make_ccs, make_crs, make_ccs_postordering
  
contains
  subroutine make_ccs(col, row)
    ! [ 1                 ]
    ! [   2               ]
    ! [     3             ]
    ! [   *   4           ]
    ! [     *   5         ]
    ! [   *     * 6       ]
    ! [ *           7     ]
    ! [ *   *   *     8   ]
    ! [   *   *   * *   9 ]
    integer, pointer, contiguous :: col(:), row(:)
    integer :: n, nonzero

    n = 9
    nonzero = 21

    allocate(col(n+1), row(nonzero))
    col = (/1, 4, 8, 11, 13, 16, 18, 20, 21, 22/)
    row = (/1, 7, 8, 2, 4, 6, 9, 3, 5, 8, 4, 9, 5, 6, 8, 6, 9, 7, 9, 8, 9/)

  end subroutine

  subroutine make_crs(row, col)
    ! [ 1                 ]
    ! [   2               ]
    ! [     3             ]
    ! [   *   4           ]
    ! [     *   5         ]
    ! [   *     * 6       ]
    ! [ *           7     ]
    ! [ *   *   *     8   ]
    ! [   *   *   * *   9 ]
    integer, pointer, contiguous :: col(:), row(:)
    integer :: n, nonzero

    n = 9
    nonzero = 21

    allocate(row(n+1), col(nonzero))
    row = (/1, 2, 3, 4, 6, 8, 11, 13, 17, 22/)
    col = (/1, 2, 3, 2, 4, 3, 5, 2, 5, 6, 1, 7, 1, 3, 5, 8, 2, 4, 6, 7, 9/)

  end subroutine

  subroutine make_ccs_postordering(col, row)
    ! [ 1                 ]
    ! [   2               ]
    ! [     3             ]
    ! [   *   4           ]
    ! [     *   5         ]
    ! [   *     * 6       ]
    ! [ *           7     ]
    ! [ *   *   *     8   ]
    ! [   *   *   * *   9 ]
    integer, pointer, contiguous :: col(:), row(:)
    integer :: n, nonzero

    n = 9
    nonzero = 21

    allocate(col(n+1), row(nonzero))
    col = (/1, 4, 6, 10, 12, 15, 18, 20, 21, 22/)
    row = (/1, 2, 8, 2, 9, 3, 4, 7, 9, 4, 9, 5, 6, 8, 6, 7, 8, 7, 9, 8, 9/)

  end subroutine
end module