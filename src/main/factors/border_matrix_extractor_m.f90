module border_matrix_extractor_m
  use matrix_extractor_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: border_extractor_c
  contains
    private
    procedure :: get_start_pos
    procedure :: get_size
    procedure :: estimate_size
  end type
contains
  integer function get_start_pos(this, node, i, j) result(pos)
    class(border_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nb, nc, nr

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)
    pos = (i-j)*nb*nb+1

  end function

  integer function get_size(this, node, i, j) result(border_size)
    class(border_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nb, nc, nr

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)
    
    border_size = get_block_size(j, nb, nc+nr)*get_block_size(i, nb, nc+nr)
    
  end function

  integer function estimate_size(this, node) result(border_size)
    class(border_extractor_c) :: this
    integer, intent(in) :: node
    integer :: nc, nr, nb
    integer :: width, sr

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

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