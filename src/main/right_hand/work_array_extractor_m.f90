module work_array_extractor_m
  use array_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: work_extractor_c
  contains
    private
    procedure :: get_start_pos
    procedure :: get_size
    procedure, public :: estimate_size
  end type
contains
  integer function get_start_pos(this, node, idx) result(pos)
    class(work_extractor_c) :: this
    integer, intent(in) :: node, idx
    integer :: nb, nc, nr
    integer :: first_block, idx2

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)
    
    idx2 = idx - nc/nb
    first_block = min(nb - mod(nc, nb), nr)
    if(idx2 == 1)then
      pos = 1
    else
      pos = first_block + (idx2-2)*nb+1
    endif

  end function

  integer function get_size(this, node, idx) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node, idx
    integer :: nb, nc, nr
    integer :: first_block, idx2

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    idx2 = idx - nc/nb
    first_block = min(nb - mod(nc, nb), nr)
    work_size = get_block_size(idx2, nb, nr, first_block)

  end function

  integer function estimate_size(this, node) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node

    work_size = this%node_data%work_size(node)
    
  end function
end module