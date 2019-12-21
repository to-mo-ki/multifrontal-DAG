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
    integer :: n, nb, col_size
    integer :: left, up

    n = this%node_data%supernode_size(node) + this%node_data%work_size(node)
    nb = this%node_data%nb
    col_size = this%node_data%get_supernode_block_size(j, node)

    left = partial_sum(n-(j-1)*nb+1, n) + partial_sum(nb-1)*(j-1)
    up = (i-j)*nb*col_size
    pos = left + up + 1

  end function

  integer function get_size(this, node, i, j) result(supernode_size)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: col_size, row_size
    
    row_size = this%node_data%get_matrix_block_size(i, node)
    col_size = this%node_data%get_supernode_block_size(j, node)
    supernode_size = row_size*col_size

  end function

  integer function estimate_size(this, node) result(supernode_size)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node
    integer :: nc, nr, nb, sn, sr

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)
    sn = nc/nb
    sr = this%node_data%border_supernode_size(node)
    
    supernode_size = partial_sum(nr+1, nc+nr) + partial_sum(nb-1)*sn+partial_sum(sr-1)
    
  end function
end module