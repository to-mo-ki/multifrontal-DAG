module supernode_matrix_extractor_m
  use matrix_extractor_m
  use partial_sum_m
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
  integer function get_start_pos(nb, nc, nr, i, j) result(pos)
    integer, intent(in) :: nb, nc, nr, i, j
    integer :: left, up

    left = partial_sum(nc+nr-(j-1)*nb+1, nc+nr) + partial_sum(1, nb-1)*(j-1)
    up = (i-j)*nb*get_block_size(j, nb, nc)
    pos = left + up + 1

  end function

  integer function get_size(nb, nc, nr, i, j) result(supernode_size)
    integer, intent(in) :: nb, nc, nr, i, j

    supernode_size = get_block_size(i, nb, nc+nr) * get_block_size(j, nb, nc)

  end function

  integer function estimate_size(nb, nc, nr) result(supernode_size)
    integer, intent(in) :: nc, nr, nb
    integer :: sn, sr

    sn = nc/nb
    sr = mod(nc, nb)
    
    supernode_size = partial_sum(nr+1, nc+nr) + partial_sum(1, nb-1)*sn+partial_sum(1, sr-1)
    
  end function
end module