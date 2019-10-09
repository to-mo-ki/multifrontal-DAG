module factors_pos_calculator_m
  use partial_sum_m
  implicit none
  private

  public :: get_supernode_pos, get_work_pos, get_border_pos
  public :: get_supernode_size, get_work_size, get_border_size
  
contains
  integer function get_supernode_pos(nb, nc, nr, i, j) result(pos)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: left, up
    
    left = partial_sum(nc+nr-(j-1)*nb+1, nc+nr) + partial_sum(1, nb-1)*(j-1)
    up = (i-j)*nb*get_block_size(j, nb, nc)
    pos = left + up + 1

  end function

  integer function get_supernode_size(nb, nc, nr, i, j) result(supernode_size)
    integer, intent(in) :: nb, nc, nr, i, j
    
    supernode_size = get_block_size(i, nb, nc+nr) * get_block_size(j, nb, nc)
    
  end function

  integer function get_work_pos(nb, nc, nr, i, j) result(pos)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: left, up
    integer :: sn, sr, fw, wn, wr

    sn = nc/nb
    sr = mod(nc, nb)
    fw = mod(nb-sr, nb)
    wn = (nr-fw)/nb
    wr = mod(nr-fw, nb)
    wn = max((j-1)-(nc+fw)/nb, 0)

    if(j == nc/nb+1)then
      up = get_block_size2(j-sn, fw, nb, nr) * max((nb*(i-1)-nc), 0)
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(1, nb-1)*wn
    else
      up = (i-j)*nb*nb
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(1, nb-1)*wn+partial_sum(1, fw-1)
    endif
    pos = left + up + 1

  end function

  integer function get_work_size(nb, nc, nr, i, j) result(work_size)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: sn, sr, fw

    sn = nc/nb
    sr = mod(nc, nb)
    fw = mod(nb-sr, nb)

    work_size = get_block_size2(i-sn, fw, nb, nr) * get_block_size2(j-sn, fw, nb, nr)
    
  end function

  integer function get_border_pos(nb, nc, nr, i, j) result(pos)
    integer, intent(in) :: nb, nc, nr, i, j
    
    pos = (i-j)*nb*nb+1

  end function

  integer function get_border_size(nb, nc, nr, i, j) result(border_size)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: sn, sr, fw
    
    border_size = nb*get_block_size(i, nb, nc+nr)

  end function

  integer function get_block_size(idx, nb, n) result(block_size)
    integer, intent(in) :: idx, nb, n

    if(idx <= n/nb)then
      block_size = nb
    else
      block_size = mod(n, nb)
    endif

  end function

  integer function get_block_size2(idx, fw, nb, n) result(block_size)
    integer, intent(in) :: idx, fw, nb, n
    if(fw == 0)then
      block_size = get_block_size(idx, nb, n)
      return
    endif
    if(idx == 1)then
      block_size = fw
      return
    endif
    block_size = get_block_size(idx-1, nb, n-fw)

  end function

end module