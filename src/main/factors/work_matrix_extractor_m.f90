module work_matrix_extractor_m
  use matrix_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: work_extractor_c
  contains
    private
    procedure :: get_start_pos
    procedure :: get_size
    procedure :: estimate_size
  end type
contains
  integer function get_start_pos(this, node, i, j) result(pos)
    class(work_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: left, up, nb, nc, nr
    integer :: fw, wn, up_col_size, up_row_size

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)
    fw = this%node_data%border_work_size(node)
    up_col_size = this%node_data%get_work_block_size(j, node)
    
    if(j == this%node_data%get_work_start_index(node))then
      up_row_size =  max((nb*(i-1)-nc), 0)
      left = 0
    else
      up_row_size = (i-j)*nb
      wn = j-this%node_data%get_work_start_index(node)-1
      left = partial_sum(nc+nr-(j-1)*nb+1, nr) + partial_sum(nb-1)*wn+partial_sum(fw-1)
    endif

    up = up_col_size*up_row_size
    pos = left + up + 1

  end function

  integer function get_size(this, node, i, j) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: col_size, row_size

    col_size = this%node_data%get_work_block_size(j, node)
    row_size = this%node_data%get_work_block_size(i, node)
    work_size = col_size*row_size

  end function

  integer function estimate_size(this, node) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node
    integer :: nc, nr, nb
    integer :: sn, wn, fw
    integer :: left, lower, internal, last_num, lower_row_size
    
    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    sn = nc/nb
    last_num = this%node_data%get_num_matrix_block(node)
    lower_row_size = this%node_data%get_matrix_block_size(last_num, node)
    fw = this%node_data%border_work_size(node)
    wn = this%node_data%get_num_work_block(node)-2
    
    left = nr*fw
    internal = partial_sum(wn)*nb*nb
    lower = (nr-fw)*lower_row_size

    work_size = left + internal + lower
    
  end function
end module