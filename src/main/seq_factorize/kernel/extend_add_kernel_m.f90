module extend_add_kernel_m
  implicit none
  
contains

  subroutine extend_add_rect(from, to, col_local, row_local, ld_from, ld_to, coffset, roffset)
    double precision, intent(in) :: from(ld_from, *)
    double precision, intent(inout) :: to(ld_to, *)
    integer, intent(in) :: col_local(:), row_local(:)
    integer, intent(in) :: ld_from, ld_to, coffset, roffset
    integer :: i, j

    do i=1, size(row_local)
      do j=1, size(col_local)
        to(col_local(j), row_local(i)) = to(col_local(j), row_local(i)) + from(coffset+j, roffset+i)
      enddo
    enddo

  end subroutine

  subroutine extend_add_tri(from, to, local, ld_from, ld_to, offset)
    double precision, intent(in) :: from(ld_from, *)
    double precision, intent(inout) :: to(ld_to, *)
    integer :: local(:)
    integer, intent(in) :: ld_from, ld_to, offset
    integer :: i, j

    do i=1, size(local)
      do j=1, i
        to(local(j), local(i)) = to(local(j), local(i)) + from(offset+j, offset+i)
      enddo
    enddo

  end subroutine

end module