module partial_sum_m
  implicit none
  
contains

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

  integer function partial_sum(s, e)
    integer, intent(in) :: s, e
    if(e < s)then
      partial_sum = 0
      return
    endif
    partial_sum = e*(e+1)/2-s*(s-1)/2
  end function
end module