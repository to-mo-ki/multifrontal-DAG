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
    procedure :: get_matrix
    procedure :: get_supernode
    procedure :: get_work
    procedure :: get_border
  end type

  public :: create_factors

contains
  function create_factors(node_data, nb)result(this)
    type(factors_c), pointer :: this
    type(node_data_c), pointer :: node_data
    integer, intent(in) :: nb
    class(matrix_controller_c), pointer :: controller
    
    allocate(this)
    allocate(supernode_controller_c::controller)
    this%supernode => create_block_matrices(nb, node_data%supernode_size, node_data%work_size, controller)
    allocate(work_controller_c::controller)
    this%work => create_block_matrices(nb, node_data%supernode_size, node_data%work_size, controller)
    allocate(border_controller_c::controller)
    this%border => create_block_matrices(nb, node_data%supernode_size, node_data%work_size, controller)
    this%nb = nb
    this%node_data => node_data
  
  end function

  function get_matrix(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    integer :: nc
    type(block_matrices_c), pointer :: block_matrices

    nc = this%node_data%get_num_supernode_block(node)
    
    if(this%node_data%divisible(node))then
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

  function get_supernode(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    
    ptr => this%supernode%get_ptr(node, i, j)
  end function

  function get_work(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    
    ptr => this%work%get_ptr(node, i, j)

  end function

  function get_border(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    
    ptr => this%border%get_ptr(node, i, j)

  end function

end module