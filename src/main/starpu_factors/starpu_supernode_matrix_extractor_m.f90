module supernode_starpu_matrix_extractor_m
  use starpu_matrix_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: supernode_extractor_c
  contains
    private
    procedure :: get_pos
    procedure, public :: estimate_size
  end type
contains
  integer function get_pos(this, node, i, j) result(ptr)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node, i, j
    
  end function

  integer function estimate_size(this, node)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node
    
  end function
end module