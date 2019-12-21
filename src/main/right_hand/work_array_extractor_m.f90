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
    integer :: nb, first_block, wstart

    wstart = this%node_data%get_work_start_index(node)
    if(idx == wstart)then
      pos = 1
      return
    endif

    nb = this%node_data%nb
    first_block = this%node_data%border_work_size(node)
    pos = first_block + (idx-wstart-1)*nb+1

  end function

  integer function get_size(this, node, idx) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node, idx

    work_size = this%node_data%get_work_block_size(idx, node)

  end function

  integer function estimate_size(this, node) result(work_size)
    class(work_extractor_c) :: this
    integer, intent(in) :: node

    work_size = this%node_data%work_size(node)
    
  end function
end module