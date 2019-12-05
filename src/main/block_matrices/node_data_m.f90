module node_data_m
  use contiguous_sets_m
  implicit none
  private
  type, public :: node_data_c
    type(contiguous_sets_c), pointer :: node_sets
    integer, public :: nb, num_node, max_num_block
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    integer, allocatable :: num_supernode_block(:), num_work_block(:)
    integer, allocatable :: border_supernode_size(:)
  contains
    procedure :: divisible
    procedure :: get_border_supernode_size
    procedure :: get_border_work_size
    procedure :: get_num_matrix_block
    procedure :: get_num_supernode_block
    procedure :: get_num_work_block
    procedure :: get_work_size
    procedure :: get_matrix_num
    procedure :: get_work_num
    procedure :: get_work_start_index
    procedure :: get_block_size
    procedure :: get_supernode_block_size
    procedure :: get_work_size2
  end type

  public :: create_node_data

contains
  function create_node_data(supernode_size, work_size, nb) result(this)
    type(node_data_c), pointer :: this
    integer, target, contiguous :: supernode_size(:), work_size(:)
    integer, intent(in) :: nb
    integer :: num_node, i, r

    allocate(this)
    this%node_sets => create_contiguous_sets(supernode_size)
    this%supernode_size => supernode_size
    this%work_size => work_size
    this%nb = nb
    num_node = size(supernode_size)
    this%num_node = num_node
    this%max_num_block = maxval(supernode_size+work_size)/nb+1

    allocate(this%border_supernode_size(num_node))
    do i=1, num_node
      this%border_supernode_size(i) = mod(supernode_size(i), nb)
    enddo

    allocate(this%num_supernode_block(num_node))
    do i=1, num_node
      if(this%divisible(i))then
        this%num_supernode_block(i) = supernode_size(i)/nb
      else
        this%num_supernode_block(i) = supernode_size(i)/nb+1
      endif
    enddo

    allocate(this%num_work_block(num_node))
    do i=1, num_node
      if(this%divisible(i))then
        r = mod(work_size(i), nb)
        if(r == 0)then
          this%num_work_block(i) = work_size(i)/nb
        else
          this%num_work_block(i) = work_size(i)/nb+1
        endif
      else
        r = mod(work_size(i)-this%get_border_work_size(i), nb)
        if(r == 0)then
          this%num_work_block(i) = (work_size(i)-this%get_border_work_size(i))/nb+1
        else
          this%num_work_block(i) = (work_size(i)-this%get_border_work_size(i))/nb+2
        endif
      endif
    enddo


  end function

  logical function divisible(this, node)
    class(node_data_c) :: this
    integer, intent(in) :: node
    divisible = this%border_supernode_size(node) == 0
  end function
  
  integer function get_border_supernode_size(this, node) result(border_supernode_size)
    class(node_data_c) :: this
    integer, intent(in) :: node
    border_supernode_size = this%border_supernode_size(node)
  end function

  integer function get_border_work_size(this, node) result(border_work_size)
    class(node_data_c) :: this
    integer, intent(in) :: node
    border_work_size = this%nb - this%border_supernode_size(node)
  end function

  integer function get_num_supernode_block(this, node) result(num_supernode_block)
    class(node_data_c) :: this
    integer, intent(in) :: node
    num_supernode_block = this%num_supernode_block(node)
  end function

  integer function get_num_work_block(this, node) result(num_work_block)
    class(node_data_c) :: this
    integer, intent(in) :: node
    num_work_block = this%num_work_block(node)
  end function


  function get_work_size(this, idx, node) result(block_size)
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, first_block_size, work_size, work_index
    integer :: n

    n = this%supernode_size(node)
    block_size = this%get_work_size2(idx-n/this%nb, node)
    
  end function

  function get_work_size2(this, idx, node) result(block_size)
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, first_block_size, work_size
    
    nb = this%nb
    first_block_size = this%get_border_work_size(node)
    work_size = this%work_size(node)
    if(idx == 1)then
      block_size = first_block_size
    else
      if((idx-1)*nb + first_block_size > work_size)then
        block_size = mod(work_size-first_block_size, nb)
      else
        block_size = nb
      endif
    endif
    
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

  ! TODO: TEST
  integer function get_num_matrix_block(this, node) result(num_block)
    class(node_data_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc, nr, n

    nb = this%nb
    nc = this%supernode_size(node)
    nr = this%work_size(node)
    n = nc + nr
    if(mod(n, nb) == 0)then
      num_block = n/nb
    else
      num_block = n/nb+1
    endif

  end function

  integer function get_work_start_index(this, node) result(idx)
    class(node_data_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc
    
    nb = this%nb
    nc = this%supernode_size(node)
    idx = nc/nb+1
    
  end function

  function get_block_size(this, idx, node) result(block_size)
    use block_size_calculator_m, p_get_block_size => get_block_size
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, n
    
    n = this%supernode_size(node)+this%work_size(node)
    nb = this%nb
    block_size = p_get_block_size(idx, nb, n)
    
  end function

  function get_supernode_block_size(this, idx, node) result(block_size)
    use block_size_calculator_m, p_get_block_size => get_block_size
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: block_size
    integer :: nb, n
    
    n = this%supernode_size(node)
    nb = this%nb
    block_size = p_get_block_size(idx, nb, n)
    
  end function

end module