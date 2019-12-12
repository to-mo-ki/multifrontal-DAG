module array_rearrange_subroutines_m
  use right_hand_m
  use array_rearrange_kernel_m
  implicit none
  private

  public :: rearrange_array_f, rearrange_array_b
  
contains
  subroutine rearrange_array_f(rh, node, idx)
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, idx
    double precision, pointer, contiguous :: border(:), supernode(:), work(:)

    border => rh%get_border(node, idx)
    supernode => rh%get_supernode(node, idx)
    work => rh%get_work(node, idx)
    call split_array(border, supernode, work, size(supernode), size(work))
    
  end subroutine

  subroutine rearrange_array_b(rh, node, idx)
    type(right_hand_c), pointer :: rh
    integer, intent(in) :: node, idx
    double precision, pointer, contiguous :: border(:), supernode(:), work(:)

    border => rh%get_border(node, idx)
    supernode => rh%get_supernode(node, idx)
    work => rh%get_work(node, idx)
    call join_array(border, supernode, work, size(supernode), size(work))
    
  end subroutine
end module