module supernode_array_extractor_m
  use array_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: supernode_extractor_c
  contains
    procedure :: get_start_pos
    procedure :: get_size
    procedure :: estimate_size
  end type
contains
  integer function get_start_pos(this, node, idx) result(pos)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node, idx
    integer :: nb

    nb = this%node_data%nb
    pos = (idx-1)*nb+1

  end function

  integer function get_size(this, node, idx) result(supernode_size)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node, idx

    supernode_size = this%node_data%get_supernode_block_size(idx, node)

  end function

  integer function estimate_size(this, node) result(supernode_size)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node

    supernode_size = this%node_data%supernode_size(node)
    
  end function
end module