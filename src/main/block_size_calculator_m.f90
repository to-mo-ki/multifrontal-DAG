module block_size_calculator_m
  implicit none
  private
  interface get_block_size
    module procedure :: get_block_size1
    module procedure :: get_block_size2
  end interface

  public :: get_block_size

contains
  integer function get_block_size1(idx, nb, n) result(block_size)
    integer, intent(in) :: idx, nb, n
    
    if(idx*nb > n)then
      block_size = mod(n, nb)
    else
      block_size = nb
    endif

  end function

  integer function get_block_size2(idx, nb, n, first_block) result(block_size)
    integer, intent(in) :: idx, nb, n, first_block
    
    if(first_block == 0)then
      block_size = get_block_size(idx, nb, n)
      return
    endif
    if(idx == 1)then
      block_size = first_block
    else
      block_size = get_block_size(idx-1, nb, n-first_block)
    endif

  end function

end module