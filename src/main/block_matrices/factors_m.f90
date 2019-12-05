module factors_m
  use contiguous_sets_m
  use jagged_array_m
  use block_matrices_m
  use matrix_controller_m
  use supernode_controller_m
  use work_controller_m
  use border_controller_m
  use node_data_m
  implicit none
  private
  type, public :: factors_c
    private
    type(node_data_c), pointer :: node_data
    type(block_matrices_c), pointer :: supernode, work, border
    integer :: nb
  contains
    procedure :: get_matrix_ptr, get_supernode_ptr, get_work_ptr, get_border_ptr
    procedure :: get_num_block, get_work_start_index, exist_border, get_num_node
    procedure :: get_block_size, get_supernode_block_size, get_border_info, get_work_size
    procedure :: get_first, get_last
  end type

  public :: create_factors

contains
  function create_factors(node_data, ccs, nb)result(this)
    type(factors_c), pointer :: this
    type(node_data_c), pointer :: node_data
    type(jagged_array_c), pointer, intent(in) :: ccs
    integer, intent(in) :: nb
    class(matrix_controller_c), pointer :: controller
    
    allocate(this)
    allocate(supernode_controller_c::controller)
    this%supernode => create_block_matrices(nb, node_data%node_sets, ccs, controller)
    allocate(work_controller_c::controller)
    this%work => create_block_matrices(nb, node_data%node_sets, ccs, controller)
    allocate(border_controller_c::controller)
    this%border => create_block_matrices(nb, node_data%node_sets, ccs, controller)
    this%nb = nb
    this%node_data => node_data
  
  end function

  function get_matrix_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nc, nr, nb, r, q
    type(block_matrices_c), pointer :: block_matrices

    nc = this%node_data%get_num_supernode_block(node)
    
    if(.not. this%exist_border(node))then
      if(j <= nc)then
        block_matrices => this%supernode
      else
        block_matrices => this%work
      endif
    else
      if(j < nc)then
        block_matrices => this%supernode
      else if(j > nc)then
        block_matrices => this%work
      else
        block_matrices => this%border
      endif
    endif
    !TODO: これがないときにエラーを発生させるテスト作成
    if(node == this%node_data%num_node)then
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

    num_block = this%node_data%get_num_matrix_block(node)

  end function

  integer function get_work_start_index(this, node) result(idx)
    class(factors_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc
    
    nb = this%nb
    nc = this%node_data%supernode_size(node)
    idx = nc/nb+1
    
  end function

  logical function exist_border(this, node)
    class(factors_c) :: this
    integer, intent(in) :: node
    
    exist_border = .not. this%node_data%divisible(node)
  
  end function

  integer function get_num_node(this) result(num_node)
    class(factors_c) :: this
    num_node = this%node_data%num_node
  end function

  function get_block_size(this, idx, node) result(block_size)
    use block_size_calculator_m, p_get_block_size => get_block_size
    class(factors_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, n
    
    n = this%node_data%supernode_size(node)+this%node_data%work_size(node)
    nb = this%nb
    block_size = p_get_block_size(idx, nb, n)
    
  end function

  function get_supernode_block_size(this, idx, node) result(block_size)
    use block_size_calculator_m, p_get_block_size => get_block_size
    class(factors_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, n
    
    n = this%node_data%supernode_size(node)
    nb = this%nb
    block_size = p_get_block_size(idx, nb, n)
    
  end function

  function get_work_size(this, idx, node) result(block_size)
    class(factors_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, first_block_size, work_size, work_index
    integer :: n

    n = this%node_data%supernode_size(node)
    block_size = this%node_data%get_work_size(idx-n/this%nb, node)
    
  end function

  subroutine get_border_info(this, node, ssize, wsize)
    class(factors_c) :: this
    integer, intent(in) :: node
    integer, intent(out) :: ssize, wsize
    integer :: order, block_size, j

    ssize = this%node_data%get_border_supernode_size(node)
    wsize = this%node_data%get_border_work_size(node)

  end subroutine

  integer function get_first(this, node)
    class(factors_c) :: this
    integer, intent(in) :: node

    get_first = this%node_data%node_sets%get_first(node)

  end function

  integer function get_last(this, node)
    class(factors_c) :: this
    integer, intent(in) :: node

    get_last = this%node_data%node_sets%get_last(node)

  end function

end module