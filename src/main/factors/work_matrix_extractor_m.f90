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
    integer :: sn, sr, fw, wn, wr

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)
    
    sn = nc/nb
    sr = mod(nc, nb)
    fw = min(mod(nb-sr, nb), nr)
    wn = (nr-fw)/nb
    wr = mod(nr-fw, nb)
    wn = max((j-1)-(nc+fw)/nb, 0)
    
    if(j == nc/nb+1)then
      up = get_block_size(j-sn, nb, nr, fw) * max((nb*(i-1)-nc), 0)
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(nb-1)*wn
    else
      up = (i-j)*nb*nb
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(nb-1)*wn+partial_sum(fw-1)
    endif
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
    integer :: sn, wn, wr, fw
    integer :: left, lower, internal, last_num, lower_row_size
    
    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    sn = nc/nb
    last_num = this%node_data%get_num_matrix_block(node)
    lower_row_size = this%node_data%get_matrix_block_size(last_num, node)
    fw = this%node_data%border_work_size(node)
    
    if(this%node_data%divisible(node))then
      wn = this%node_data%get_num_matrix_block(node)-sn-1
      left = 0
      internal = partial_sum(wn)*nb*nb
      lower = lower_row_size*nr
    else
      wn = this%node_data%get_num_matrix_block(node)-sn-2
      left = nr*fw
      internal = partial_sum(wn)*nb*nb
      lower = (nr-fw)*lower_row_size
    endif

    work_size = left + internal + lower
    
  end function
end module