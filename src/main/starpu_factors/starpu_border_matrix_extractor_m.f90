module border_starpu_matrix_extractor_m
  use starpu_matrix_extractor_m
  use integer_function_m
  implicit none
  private
  type, extends(extractor_c), public :: border_extractor_c
  contains
    procedure :: get_pos
    procedure :: estimate_size
  end type
contains
  integer function get_pos(this, node, i, j) result(ptr)
    class(border_extractor_c) :: this
    integer, intent(in) :: node, i, j

    ptr = i - j + 1
    
  end function

  integer function estimate_size(this, node)
    class(border_extractor_c) :: this
    integer, intent(in) :: node
    integer :: num_block, border_index
    if(.not. this%node_data%exist_border(node))then
      estimate_size = 0
      return
    endif
    num_block = this%node_data%get_num_matrix_block(node)
    border_index = this%node_data%get_work_start_index(node)
    estimate_size = num_block - border_index + 1

  end function
end module