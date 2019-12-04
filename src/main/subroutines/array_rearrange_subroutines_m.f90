module array_rearrange_subroutines_m
  use right_hand_m
  use array_splitter_m
  implicit none
  private

  public :: rearrange_array
  
contains
  subroutine rearrange_array(rh, node, idx)
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, idx
    double precision, pointer, contiguous :: border(:), supernode(:), work(:)

    border => rh%get_border_ptr(node, idx)
    supernode => rh%get_supernode_ptr(node, idx)
    work => rh%get_work_ptr(node, idx)
    call split_array(border, supernode, work, size(supernode), size(work))
    
  end subroutine
end module