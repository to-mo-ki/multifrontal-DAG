module work_array_extractor_m
  use array_extractor_m
  use partial_sum_m
  use block_size_calculator_m
  implicit none
  private
  type, extends(extractor_c), public :: work_extractor_c
  contains
    private
    procedure, nopass :: get_start_pos
    procedure, nopass :: get_size
    procedure, nopass, public :: estimate_size
  end type
contains
  integer function get_start_pos(nb, nc, nr, idx) result(pos)
    integer, intent(in) :: nb, nc, nr, idx
    integer :: first_block, idx2
    
    idx2 = idx - nc/nb
    first_block = min(nb - mod(nc, nb), nr)
    if(idx2 == 1)then
      pos = 1
    else
      pos = first_block + (idx2-2)*nb+1
    endif

  end function

  integer function get_size(nb, nc, nr, idx) result(work_size)
    integer, intent(in) :: nb, nc, nr, idx
    integer :: first_block, idx2

    idx2 = idx - nc/nb
    first_block = min(nb - mod(nc, nb), nr)
    work_size = get_block_size(idx2, nb, nr, first_block)

  end function

  integer function estimate_size(nb, nc, nr) result(work_size)
    integer, intent(in) :: nc, nr, nb

    work_size = nr
    
  end function
end module