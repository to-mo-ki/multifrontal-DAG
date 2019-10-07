module factors_size_estimator_m
  use partial_sum_m
  implicit none
  private
  
  public :: estimate_supernode_size, estimate_border_size, estimate_work_size

contains
  integer function estimate_supernode_size(nc, nr, nb)result(supernode_size)
    integer, intent(in) :: nc, nr, nb
    integer :: sn, sr

    sn = nc/nb
    sr = mod(nc, nb)
    !print *, partial_sum(nr+1, nc+nr), partial_sum(1, nb-1), sn, partial_sum(1, sr-1)
    supernode_size = partial_sum(nr+1, nc+nr) + partial_sum(1, nb-1)*sn+partial_sum(1, sr-1)
    
  end function

  integer function estimate_work_size(nc, nr, nb) result(work_size)
    integer, intent(in) :: nc, nr, nb
    integer :: sn, sr, wn, wr, fw
    sn = nc/nb
    sr = mod(nc, nb)
    fw = mod(nb-sr, nb)
    wn = (nr-fw)/nb
    wr = mod(nr-fw, nb)
    work_size = partial_sum(1, nr) + partial_sum(1, nb-1)*wn
    !print *, partial_sum(1, nr), partial_sum(1, nb-1)*wn, wn
    work_size = work_size + partial_sum(1, fw-1)
    !print *, partial_sum(1, nb-sr-1)
    work_size = work_size + partial_sum(1, wr-1)
    !print *, partial_sum(1, wr-1), wr
    
  end function

  integer function estimate_border_size(nc, nr, nb) result(border_size)
    integer, intent(in) :: nc, nr, nb
    if(mod(nc, nb) == 0)then
      border_size = 0
    else
      border_size = (nc+nr-(nc/nb)*nb)*nb
    endif

  end function
end module