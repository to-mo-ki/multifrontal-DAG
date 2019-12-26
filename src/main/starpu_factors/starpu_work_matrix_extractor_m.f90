module work_starpu_matrix_extractor_m
  use starpu_matrix_extractor_m
  use integer_function_m
  implicit none
  private
  type, extends(extractor_c), public :: work_extractor_c
  contains
    procedure :: get_pos
    procedure :: estimate_size
  end type
contains
  integer function get_pos(this, node, i, j) result(ptr)
    class(work_extractor_c) :: this
    integer, intent(in) :: node, i, j
    integer :: num_matrix, num_work, work_start
    
    num_matrix = this%node_data%get_num_matrix_block(node)
    work_start = this%node_data%get_work_start_index(node)
    num_work = num_matrix - work_start + 1
    ptr = triangular_pos(i-work_start+1, j-work_start+1, num_work)
    
  end function

  integer function estimate_size(this, node)
    class(work_extractor_c) :: this
    integer, intent(in) :: node
    integer :: num_block, work_start

    num_block = this%node_data%get_num_matrix_block(node)
    work_start = this%node_data%get_work_start_index(node)
    estimate_size = partial_sum(num_block-work_start+1)
    
  end function
end module