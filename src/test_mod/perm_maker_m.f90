module perm_maker_m
  implicit none
  
contains
  subroutine make_postordering_perm(perm, iperm)
    integer, pointer, contiguous, optional :: perm(:), iperm(:)
    if(present(perm))then
      allocate(perm(9))
      perm = (/1, 7, 2, 4, 3, 5, 6, 8, 9/)
    endif
    if(present(iperm))then
      allocate(iperm(9))
      iperm = (/1, 3, 5, 4, 6, 7, 2, 8, 9/)
    endif
  end subroutine

end module