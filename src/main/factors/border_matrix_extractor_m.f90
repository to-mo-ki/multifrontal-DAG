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
    procedure, public :: estimate_size
  end type
contains
  integer function get_start_pos(this, node, i, j) result(pos)
    class(border_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nb

    nb = this%node_data%nb
    pos = (i-j)*nb*nb+1

  end function

  integer function get_size(this, node, i, j) result(border_size)
    class(border_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: col_size, row_size

    col_size = this%node_data%get_matrix_block_size(j, node)
    row_size = this%node_data%get_matrix_block_size(i, node)
    border_size = col_size * row_size
    
  end function

  integer function estimate_size(this, node) result(border_size)
    class(border_extractor_c) :: this
    integer, intent(in) :: node
    integer :: nc, nr, nb
    integer :: row_size, j, col_size

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    if(.not. this%node_data%exist_border(node))then
      border_size = 0
      return
    endif
    
    j = this%node_data%get_work_start_index(node)
    col_size = this%node_data%get_matrix_block_size(j, node)
    row_size = nc+nr-(nc/nb)*nb
    border_size = row_size*col_size
    
  end function
end module