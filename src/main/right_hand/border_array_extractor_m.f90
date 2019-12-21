module border_array_extractor_m
  use array_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: border_extractor_c
  contains
    private
    procedure :: get_start_pos
    procedure :: get_size
    procedure, public :: estimate_size
  end type
contains
  integer function get_start_pos(this, node, idx) result(pos)
    class(border_extractor_c) :: this
    integer, intent(in) :: node, idx

    pos = 1

  end function

  integer function get_size(this, node, idx) result(border_size)
    class(border_extractor_c) :: this
    integer, intent(in) :: node, idx

    border_size = get_block_size(idx, this%node_data%nb, this%node_data%supernode_size(node)+this%node_data%work_size(node))

  end function

  integer function estimate_size(this, node) result(border_size)
    class(border_extractor_c) :: this
    integer, intent(in) :: node
    integer :: nb, nc, nr, idx

    nb = this%node_data%nb
    nc = this%node_data%supernode_size(node)
    nr = this%node_data%work_size(node)

    if(mod(nc, nb)==0)then
      border_size = 0
      return
    else
      idx = nc/nb+1
    endif

    border_size = get_block_size(idx, nb, nc+nr)
    
  end function
end module