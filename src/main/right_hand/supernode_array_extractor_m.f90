module supernode_array_extractor_m
  use array_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: supernode_extractor_c
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