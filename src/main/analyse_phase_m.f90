module analyze_phase_m
  use jagged_array_m
  use ccs_to_crs_m
  use compute_tree_m
  implicit none
  private
  
contains
  subroutine analyze_phase(origin_ccs)
    ! NOTE: reorderingは外部で行う
    type(jagged_array_c), intent(in) :: origin_ccs
    type(jagged_array_c) :: origin_crs
    integer, pointer, contiguous :: parent(:)

    call ccs_to_crs(origin_ccs, origin_crs)
    parent => compute_tree(origin_crs)
    

  end subroutine
end module