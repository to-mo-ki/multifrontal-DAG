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
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(1, nb-1)*wn
    else
      up = (i-j)*nb*nb
      left = partial_sum(nc+nr-((j-1)*nb-1), nr) + partial_sum(1, nb-1)*wn+partial_sum(1, fw-1)
    endif
    pos = left + up + 1

  end function

  integer function get_size(this, node, i, j) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nb, nc, nr
    integer :: sn, sr, fw

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    sn = nc/nb
    sr = mod(nc, nb)
    fw = min(mod(nb-sr, nb), nr)

    work_size = get_block_size(i-sn, nb, nr, fw) * get_block_size(j-sn, nb, nr, fw)
    
  end function

  integer function estimate_size(this, node) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node
    integer :: nc, nr, nb
    integer :: sn, sr, wn, wr, fw
    integer :: left, lower, internal
    
    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    sn = nc/nb
    sr = mod(nc, nb)
    fw = min(mod(nb-sr, nb), nr)
    wn = (nr-fw)/nb
    wr = mod(nr-fw, nb)
    
    left = nr*fw
    internal = partial_sum(1, wn)*nb*nb
    lower = (nr-fw)*wr
    work_size = left + internal + lower
    
  end function
end module