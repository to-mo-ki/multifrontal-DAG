module supernode_starpu_matrix_extractor_m
  use starpu_matrix_extractor_m
  use integer_function_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: supernode_extractor_c
  contains
    procedure :: get_pos
    procedure :: estimate_size
  end type
contains
  integer function get_pos(this, node, i, j) result(ptr)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: num_block

    num_block = this%node_data%get_num_matrix_block(node)
    ptr = triangular_pos(i, j, num_block)
    
  end function

  integer function estimate_size(this, node)
    class(supernode_extractor_c) :: this
    integer, intent(in) :: node
    integer :: num_matrix, last, num_last
    
    num_matrix = this%node_data%get_num_matrix_block(node)
    last = this%node_data%get_num_supernode_block(node)
    num_last = num_matrix - last + 1
    estimate_size = partial_sum(num_last, num_matrix)

  end function
end module