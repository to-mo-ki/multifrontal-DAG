module factors_m
  use contiguous_sets_m
  use jagged_array_m
  use block_matrices_m
  use matrix_controller_m
  use supernode_controller_m
  use work_controller_m
  use border_controller_m
  implicit none
  private
  type, public :: factors_c
    private
    type(block_matrices_c), pointer :: supernode, work, border
    type(contiguous_sets_c), pointer :: node_sets
    type(jagged_array_c), pointer :: ccs
    integer :: nb
  contains
    procedure :: get_matrix_ptr, get_supernode_ptr, get_work_ptr, get_border_ptr
    procedure :: get_num_block, get_work_start_index, exist_border, get_num_node
    procedure :: get_block_size, get_border_info, get_work_size
    procedure :: get_first, get_last
  end type

  public :: create_factors

contains
  function create_factors(node_sets, ccs, nb)result(this)
    type(factors_c), pointer :: this
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    type(jagged_array_c), pointer, intent(in) :: ccs
    integer, intent(in) :: nb
    class(matrix_controller_c), pointer :: controller
    
    allocate(this)
    allocate(supernode_controller_c::controller)
    this%supernode => create_block_matrices(nb, node_sets, ccs, controller)
    allocate(work_controller_c::controller)
    this%work => create_block_matrices(nb, node_sets, ccs, controller)
    allocate(border_controller_c::controller)
    this%border => create_block_matrices(nb, node_sets, ccs, controller)
    this%node_sets => node_sets
    this%ccs => ccs
    this%nb = nb
  
  end function

  function get_matrix_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nc, nr, nb, r, q
    type(block_matrices_c), pointer :: block_matrices
    nb = this%nb
    nc = this%node_sets%get_length(node)
    nr = this%ccs%get_array_length(node)

    r = mod(nc, nb)
    q = nc/nb
    if(r == 0)then
      if(j <= q)then
        block_matrices => this%supernode
      else
        block_matrices => this%work
      endif
    else
      if(j < q+1)then
        block_matrices => this%supernode
      else if(j > q+1)then
        block_matrices => this%work
      else
        block_matrices => this%border
      endif
    endif
    !TODO: これがないときにエラーを発生させるテスト作成
    if(node == this%node_sets%get_num_sets())then
      block_matrices => this%supernode
    endif
    ptr => block_matrices%get_ptr(node, i, j)
    
  end function

  function get_supernode_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    
    ptr => this%supernode%get_ptr(node, i, j)
  end function

  function get_work_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    
    ptr => this%work%get_ptr(node, i, j)

  end function

  function get_border_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    
    ptr => this%border%get_ptr(node, i, j)

  end function

  integer function get_num_block(this, node) result(num_block)
    class(factors_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc, nr, n

    nb = this%nb
    nc = this%node_sets%get_length(node)
    nr = this%ccs%get_array_length(node)
    n = nc + nr
    if(mod(n, nb) == 0)then
      num_block = n/nb
    else
      num_block = n/nb+1
    endif

  end function

  integer function get_work_start_index(this, node) result(idx)
    class(factors_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc
    
    nb = this%nb
    nc = this%node_sets%get_length(node)
    idx = nc/nb+1
    
  end function

  logical function exist_border(this, node)
    class(factors_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc

    nb = this%nb
    nc = this%node_sets%get_length(node)
    exist_border = mod(nc, nb) /= 0
  
  end function

  integer function get_num_node(this) result(num_node)
    class(factors_c) :: this
    num_node = this%node_sets%get_num_sets()
  end function

  function get_block_size(this, idx, node) result(block_size)
    class(factors_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, n
    
    n = this%node_sets%get_length(node) + this%ccs%get_array_length(node)
    nb = this%nb
    
    if(idx*nb > n)then
      block_size = mod(n, nb)
    else
      block_size = nb
    endif
    
  end function

  function get_work_size(this, idx, node) result(block_size)
    class(factors_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, first_block_size, work_size, work_index
    
    nb = this%nb
    first_block_size = nb - mod(this%node_sets%get_length(node), nb)
    work_size = this%ccs%get_array_length(node)
    if(idx == this%get_work_start_index(node))then
      block_size = first_block_size
    else
      !ここがおかしい
      work_index = idx - this%node_sets%get_length(node)/nb
      if((work_index-1)*nb + first_block_size > work_size)then
        block_size = mod(work_size-first_block_size, nb)
      else
        block_size = nb
      endif
    endif
    
  end function

  subroutine get_border_info(this, node, ssize, wsize)
    class(factors_c) :: this
    integer, intent(in) :: node
    integer, intent(out) :: ssize, wsize
    integer :: order, block_size, j

    j = this%get_work_start_index(node)
    block_size = this%get_block_size(j, node)
    order = this%node_sets%get_length(node)
    ssize = mod(order, this%nb)
    wsize = block_size - ssize

  end subroutine

  integer function get_first(this, node)
    class(factors_c) :: this
    integer, intent(in) :: node

    get_first = this%node_sets%get_first(node)

  end function

  integer function get_last(this, node)
    class(factors_c) :: this
    integer, intent(in) :: node

    get_last = this%node_sets%get_last(node)

  end function

end module