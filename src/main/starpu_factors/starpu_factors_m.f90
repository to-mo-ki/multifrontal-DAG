module starpu_factors_m
  use contiguous_sets_m
  use jagged_array_m
  use block_matrix_ptrs_m
  use node_data_m
  use iso_c_binding
  use extractor_types_m
  implicit none
  private
  type, public :: starpu_factors_c
    private
    type(node_data_c), pointer :: node_data
    type(block_matrix_ptrs_c), pointer :: supernode, work, border
  contains
    procedure :: get_matrix
    procedure :: get_supernode
    procedure :: get_work
    procedure :: get_border
  end type

  public :: create_starpu_factors

contains
  function create_starpu_factors(node_data)result(this)
    type(starpu_factors_c), pointer :: this
    type(node_data_c), pointer :: node_data
    
    allocate(this)
    this%supernode => create_block_matrix_ptrs(node_data, SUPERNODE_EXTRACTOR)
    this%work => create_block_matrix_ptrs(node_data, WORK_EXTRACTOR)
    this%border => create_block_matrix_ptrs(node_data, BORDER_EXTRACTOR)
    this%node_data => node_data
  
  end function

  function get_matrix(this, node, i, j) result(ptr)
    class(starpu_factors_c) :: this
    type(c_ptr), pointer :: ptr
    integer, intent(in) :: node, i, j
    integer :: nc
    type(block_matrix_ptrs_c), pointer :: block_matrix_ptrs

    if(this%node_data%divisible(node))then
      nc = this%node_data%get_work_start_index(node)-1
      if(j <= nc)then
        block_matrix_ptrs => this%supernode
      else
        block_matrix_ptrs => this%work
      endif
    else
      nc = this%node_data%get_work_start_index(node)
      if(j < nc)then
        block_matrix_ptrs => this%supernode
      else if(j > nc)then
        block_matrix_ptrs => this%work
      else
        block_matrix_ptrs => this%border
      endif
    endif
    ptr => block_matrix_ptrs%get(node, i, j)
    
  end function

  function get_supernode(this, node, i, j) result(ptr)
    class(starpu_factors_c) :: this
    type(c_ptr), pointer :: ptr
    integer, intent(in) :: node, i, j
    
    ptr => this%supernode%get(node, i, j)
  end function

  function get_work(this, node, i, j) result(ptr)
    class(starpu_factors_c) :: this
    type(c_ptr), pointer :: ptr
    integer, intent(in) :: node, i, j
    
    ptr => this%work%get(node, i, j)

  end function

  function get_border(this, node, i, j) result(ptr)
    class(starpu_factors_c) :: this
    type(c_ptr), pointer :: ptr
    integer, intent(in) :: node, i, j
    
    ptr => this%border%get(node, i, j)

  end function

end module