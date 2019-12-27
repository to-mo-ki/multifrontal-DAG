module factors_m
  use contiguous_sets_m
  use jagged_array_m
  use block_matrices_m
  use node_data_m
  use extractor_types_m
  implicit none
  private
  type, public :: factors_c
    private
    type(node_data_c), pointer :: node_data
    type(block_matrices_c), pointer :: supernode, work, border
  contains
    procedure :: get_matrix
    procedure :: get_supernode
    procedure :: get_work
    procedure :: get_border
  end type

  public :: create_factors

contains
  function create_factors(node_data)result(this)
    type(factors_c), pointer :: this
    type(node_data_c), pointer :: node_data
    
    allocate(this)
    this%supernode => create_block_matrices(node_data, SUPERNODE_EXTRACTOR)
    this%work => create_block_matrices(node_data, WORK_EXTRACTOR)
    this%border => create_block_matrices(node_data, BORDER_EXTRACTOR)
    this%node_data => node_data
  
  end function

  function get_matrix(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(factors_c) :: this
    integer, intent(in) :: node, i, j
    type(block_matrices_c), pointer :: block_matrices

    select case(this%node_data%get_extractor_type(j, node))
      case(SUPERNODE_EXTRACTOR)
        block_matrices => this%supernode
      case(BORDER_EXTRACTOR)
        block_matrices => this%border
      case(WORK_EXTRACTOR)
        block_matrices => this%work
    end select
    
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