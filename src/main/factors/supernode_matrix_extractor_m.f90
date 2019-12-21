module supernode_matrix_extractor_m
  use matrix_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: supernode_extractor_c
  contains
    private
    procedure :: get_start_pos
    procedure :: get_size
    procedure :: estimate_size
  end type
contains
  integer function get_start_pos(this, node, i, j) result(pos)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: left, up, nb, nc, nr

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    left = partial_sum(nc+nr-(j-1)*nb+1, nc+nr) + partial_sum(nb-1)*(j-1)
    up = (i-j)*nb*get_block_size(j, nb, nc)
    pos = left + up + 1

  end function

  integer function get_size(this, node, i, j) result(supernode_size)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nb, nc, nr
    
    
    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)
    supernode_size = get_block_size(i, nb, nc+nr) * get_block_size(j, nb, nc)

  end function

  integer function estimate_size(this, node) result(supernode_size)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node
    integer :: nc, nr, nb
    integer :: sn, sr

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    sn = nc/nb
    sr = mod(nc, nb)
    
    supernode_size = partial_sum(nr+1, nc+nr) + partial_sum(nb-1)*sn+partial_sum(sr-1)
    
  end function
end module