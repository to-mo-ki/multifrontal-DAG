module border_kernel_m
  use factorize_kernel_m
  implicit none
  private
  public :: border_potrf, border_trsm

contains

  subroutine border_potrf(supernode, work, supernode_size, work_size)
    double precision, pointer, contiguous :: work(:), supernode(:)
    integer, intent(in) :: supernode_size, work_size
    integer :: info, diag_size

    diag_size = supernode_size*supernode_size
    call mydpotrf(supernode_size, supernode(1:diag_size), info)
    call mydtrsm(supernode_size, work_size, supernode(1:diag_size), supernode(diag_size+1:))
    call mydsyrk(work_size, supernode_size, supernode(diag_size+1:), work)

  end subroutine

  subroutine border_trsm(diag_supernode, solve_supernode, solve_work, supernode_size, work_size, nrow)
    double precision, pointer, contiguous :: diag_supernode(:), solve_work(:), solve_supernode(:)
    integer, intent(in) :: supernode_size, work_size, nrow
    integer :: diag_size

    diag_size = supernode_size*supernode_size
    call mydtrsm(supernode_size, nrow, diag_supernode(1:diag_size), solve_supernode)
    call mydgemm(supernode_size, nrow, work_size, solve_supernode, diag_supernode(diag_size+1:), solve_work)

  end subroutine

  
end module