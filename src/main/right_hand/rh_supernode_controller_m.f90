module rh_supernode_controller_m
  use rh_controller_m
  use partial_sum_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(rh_controller_c), public :: rh_supernode_controller_c
  contains
    private
    procedure, nopass :: get_start_pos
    procedure, nopass :: get_size
    procedure, nopass, public :: estimate_size
  end type
contains
  integer function get_start_pos(nb, nc, nr, idx) result(pos)
    integer, intent(in) :: nb, nc, nr, idx

    pos = (idx-1)*nb+1

  end function

  integer function get_size(nb, nc, nr, idx) result(supernode_size)
    integer, intent(in) :: nb, nc, nr, idx

    supernode_size = get_block_size(idx, nb, nc)

  end function

  integer function estimate_size(nb, nc, nr) result(supernode_size)
    integer, intent(in) :: nc, nr, nb

    supernode_size = nc
    
  end function
end module