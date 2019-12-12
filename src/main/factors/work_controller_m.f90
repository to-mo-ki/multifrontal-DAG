module work_controller_m
  use matrix_controller_m
  use partial_sum_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(matrix_controller_c), public :: work_controller_c
  contains
    private
    procedure, nopass :: get_start_pos
    procedure, nopass :: get_size
    procedure, nopass :: estimate_size
  end type
contains
    integer function get_start_pos(nb, nc, nr, i, j) result(pos)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: left, up
    integer :: sn, sr, fw, wn, wr
    
    sn = nc/nb
    sr = mod(nc, nb)
    fw = min(mod(nb-sr, nb), nr)
    wn = (nr-fw)/nb
    wr = mod(nr-fw, nb)
    wn = max((j-1)-(nc+fw)/nb, 0)
    
    if(j == nc/nb+1)then
      up = get_block_size(j-sn, nb, nr, fw) * max((nb*(i-1)-nc), 0)
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(1, nb-1)*wn
    else
      up = (i-j)*nb*nb
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(1, nb-1)*wn+partial_sum(1, fw-1)
    endif
    pos = left + up + 1

  end function

  integer function get_size(nb, nc, nr, i, j) result(work_size)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: sn, sr, fw

    sn = nc/nb
    sr = mod(nc, nb)
    fw = min(mod(nb-sr, nb), nr)

    work_size = get_block_size(i-sn, nb, nr, fw) * get_block_size(j-sn, nb, nr, fw)
    
  end function

  integer function estimate_size(nb, nc, nr) result(work_size)
    integer, intent(in) :: nc, nr, nb
    integer :: sn, sr, wn, wr, fw
    integer :: left, lower, internal
    
    sn = nc/nb
    sr = mod(nc, nb)
    fw = min(mod(nb-sr, nb), nr)
    wn = (nr-fw)/nb
    wr = mod(nr-fw, nb)
    
    left = nr*fw
    internal = partial_sum(1, wn)*nb*nb
    lower = (nr-fw)*wr
    work_size = left + internal + lower
    
  end function
end module