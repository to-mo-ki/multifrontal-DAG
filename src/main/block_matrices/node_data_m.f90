module node_data_m
  implicit none
  private
  type, public :: node_data_c
    integer, public :: nb
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    integer, allocatable :: num_supernode_block(:), num_work_block(:)
    integer, allocatable :: border_supernode_size(:)
  contains
    procedure :: divisible
    procedure :: get_border_supernode_size
    procedure :: get_border_work_size
    procedure :: get_num_supernode_block
    procedure :: get_num_work_block
    procedure :: get_matrix_block_size
  end type

  public :: create_node_data

contains
  function create_node_data(supernode_size, work_size, nb) result(this)
    type(node_data_c), pointer :: this
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    integer, intent(in) :: nb
    integer :: num_node, i, r

    allocate(this)
    this%supernode_size => supernode_size
    this%work_size => work_size
    this%nb = nb
    num_node = size(supernode_size)

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

  integer function get_matrix_block_size(this, idx, node) result(block_size)
    use block_size_calculator_m
    class(node_data_c) :: this
    integer, intent(in) :: idx, node
    integer :: n
    
    n = this%supernode_size(node) + this%work_size(node)
!    call get_block_size(idx, this%nb, n)

  end function


end module