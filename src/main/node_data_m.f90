module node_data_m
  use contiguous_sets_m
  use block_size_calculator_m
  use integer_function_m
  use extractor_types_m
  implicit none
  private
  type, public :: node_data_c
    type(contiguous_sets_c), pointer :: node_sets
    integer, public :: nb, num_node, max_num_block
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    integer, allocatable :: border_supernode_size(:), border_work_size(:)
  contains
    procedure :: divisible
    procedure :: get_num_matrix_block
    procedure :: get_num_supernode_block
    procedure :: get_num_work_block
    procedure :: get_matrix_num
    procedure :: get_work_num
    procedure :: get_work_start_index
    procedure :: get_matrix_block_size
    procedure :: get_supernode_block_size
    procedure :: get_work_block_size
    procedure :: get_extractor_type
    procedure :: exist_border
  end type

  public :: create_node_data

contains
  function create_node_data(supernode_size, work_size, nb) result(this)
    type(node_data_c), pointer :: this
    integer, target, contiguous :: supernode_size(:), work_size(:)
    integer, intent(in) :: nb
    integer :: num_node, i

    allocate(this)
    this%node_sets => create_contiguous_sets(supernode_size)
    this%supernode_size => supernode_size
    this%work_size => work_size
    this%nb = nb
    num_node = size(supernode_size)
    this%num_node = num_node
    this%max_num_block = maxval(supernode_size+work_size)/nb+1

    allocate(this%border_supernode_size(num_node), this%border_work_size(num_node))
    do i=1, num_node
      this%border_supernode_size(i) = mod(supernode_size(i), nb)
      this%border_work_size(i) = min(nb-this%border_supernode_size(i), work_size(i))
    enddo

  end function

  logical function divisible(this, node)
    class(node_data_c) :: this
    integer, intent(in) :: node
    divisible = this%border_supernode_size(node) == 0
  end function

  integer function get_matrix_num(this, idx) result(num)
    class(node_data_c) :: this
    integer, intent(in) :: idx
    
    num = (idx-1)/this%nb + 1
    
  end function

  integer function get_work_num(this, idx, node) result(num)
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    
    num = this%get_matrix_num(this%supernode_size(node) + idx)

  end function

  integer function get_num_matrix_block(this, node) result(num_block)
    class(node_data_c) :: this
    integer, intent(in) :: node
    integer :: n
    
    n = this%supernode_size(node)+this%work_size(node)
    num_block = div_ceiling(n, this%nb)
    
  end function

  integer function get_num_supernode_block(this, node) result(num_block)
    class(node_data_c) :: this
    integer, intent(in) :: node
    
    num_block = div_ceiling(this%supernode_size(node), this%nb)
    
  end function

  integer function get_num_work_block(this, node) result(num_block)
    class(node_data_c) :: this
    integer, intent(in) :: node
    
    num_block = this%get_num_matrix_block(node)-this%get_work_start_index(node)+1
    
  end function

  integer function get_work_start_index(this, node) result(idx)
    class(node_data_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc
    
    nb = this%nb
    nc = this%supernode_size(node)
    idx = nc/nb+1
    !TODO: TEST
    if(this%work_size(node) == 0)then
      idx = div_ceiling(nc, nb)+1
    endif
    
  end function

  function get_matrix_block_size(this, idx, node) result(block_size)
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, n
    
    n = this%supernode_size(node)+this%work_size(node)
    nb = this%nb
    block_size = get_block_size(idx, nb, n)
    
  end function

  function get_supernode_block_size(this, idx, node) result(block_size)
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, n
    
    n = this%supernode_size(node)
    nb = this%nb
    block_size = get_block_size(idx, nb, n)
    
  end function

  function get_work_block_size(this, idx, node) result(block_size)
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: n

    n = this%supernode_size(node)+this%work_size(node)
    if(idx == this%get_work_start_index(node))then
      block_size = this%border_work_size(node)
    else
      block_size = get_block_size(idx, this%nb, n)
    endif

  end function

  integer function get_extractor_type(this, idx, node) result(extractor_type)
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: nc

    if(this%divisible(node))then
      nc = this%get_work_start_index(node)-1
      if(idx <= nc)then
        extractor_type = SUPERNODE_EXTRACTOR
      else
        extractor_type = WORK_EXTRACTOR
      endif
    else
      nc = this%get_work_start_index(node)
      if(idx < nc)then
        extractor_type = SUPERNODE_EXTRACTOR
      else if(idx > nc)then
        extractor_type = WORK_EXTRACTOR
      else
        extractor_type = BORDER_EXTRACTOR
      endif
    endif
  end function

  logical function exist_border(this, node)
    class(node_data_c) :: this
    integer, intent(in) :: node
    exist_border = .not. this%divisible(node) .and. this%work_size(node) /= 0
  end function

end module