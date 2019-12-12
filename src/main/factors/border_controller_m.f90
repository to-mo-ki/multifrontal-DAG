module border_controller_m
  use matrix_controller_m
  use block_size_calculator_m
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
    integer :: width, sr

    sr = mod(nc, nb)

    if(mod(nc, nb) == 0)then
      border_size = 0
    else
      if(nc+nr < nb)then
        width = nc+nr
        if(width == 0)then
          width = nb
        endif
        border_size = width*width
      else
        if(nr < nb-sr)then
          border_size = (nc+nr-(nc/nb)*nb)*(sr+nr)
        else
          border_size = (nc+nr-(nc/nb)*nb)*nb
        endif
      endif
    endif
    
  end function
end module