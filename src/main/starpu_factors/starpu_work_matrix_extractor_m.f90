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
    
  end function

  integer function estimate_size(this, node)
    class(work_extractor_c) :: this
    integer, intent(in) :: node

    
  end function
end module