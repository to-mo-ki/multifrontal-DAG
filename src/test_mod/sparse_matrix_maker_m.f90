module sparse_matrix_maker_m
  implicit none
  private

  public :: make_ccs
  
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
end module