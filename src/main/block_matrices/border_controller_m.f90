module border_controller_m
  use matrix_controller_m
  use partial_sum_m
  implicit none
  private
  type, extends(matrix_controller_c), public :: border_controller_c
  contains
    private
    procedure, nopass :: get_start_pos
    procedure, nopass :: get_size
    procedure, nopass :: estimate_size
  end type
contains
  integer function get_start_pos(nb, nc, nr, i, j) result(pos)
    integer, intent(in) :: nb, nc, nr, i, j
    
    pos = (i-j)*nb*nb+1

  end function

  integer function get_size(nb, nc, nr, i, j) result(border_size)
    integer, intent(in) :: nb, nc, nr, i, j
    
    border_size = get_block_size(j, nb, nc+nr)*get_block_size(i, nb, nc+nr)
    
  end function

  integer function estimate_size(nb, nc, nr) result(border_size)
    integer, intent(in) :: nc, nr, nb
    integer :: num_block

    if(mod(nc, nb) == 0)then
      border_size = 0
    else
      if(nr < nb)then
        border_size = mod(nc+nr, nb)*mod(nc+nr, nb)
      else
        border_size = (nc+nr-(nc/nb)*nb)*nb
      endif
    endif
    
  end function
end module