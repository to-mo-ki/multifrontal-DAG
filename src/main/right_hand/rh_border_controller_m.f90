module rh_border_matrix_extractor_m
  use rh_controller_m
  use partial_sum_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(rh_controller_c), public :: rh_border_extractor_c
  contains
    private
    procedure, nopass :: get_start_pos
    procedure, nopass :: get_size
    procedure, nopass, public :: estimate_size
  end type
contains
  integer function get_start_pos(nb, nc, nr, idx) result(pos)
    integer, intent(in) :: nb, nc, nr, idx

    pos = 1

  end function

  integer function get_size(nb, nc, nr, idx) result(border_size)
    integer, intent(in) :: nb, nc, nr, idx

    border_size = get_block_size(idx, nb, nc+nr)

  end function

  integer function estimate_size(nb, nc, nr) result(border_size)
    integer, intent(in) :: nc, nr, nb
    integer :: idx

    if(mod(nc, nb)==0)then
      border_size = 0
      return
    else
      idx = nc/nb+1
    endif

    border_size = get_block_size(idx, nb, nc+nr)
    
  end function
end module