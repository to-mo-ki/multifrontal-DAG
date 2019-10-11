module action_m
  use test_util
  implicit none
  
  interface
    subroutine action_i(matrix, i, j, node)
      double precision, pointer, contiguous :: matrix(:)
      integer, intent(in) :: i, j, node
    end subroutine
  end interface
contains

  subroutine a_set_zero(matrix, i, j, node)
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: i, j, node
    matrix = 0.0d0
  end subroutine

  subroutine a_set_mark(matrix, i, j, node)
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: i, j, node
    matrix = matrix + 1.0d0
  end subroutine

  subroutine a_check_mark(matrix, i, j, node)
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: i, j, node
    double precision, allocatable :: check(:)
    allocate(check(size(matrix)))
    check = 1d0
    call assert_equal("passed: node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), matrix, check)
  end subroutine

  subroutine a_check_no_mark(matrix, i, j, node)
    double precision, pointer, contiguous :: matrix(:)
    integer, intent(in) :: i, j, node
    double precision, allocatable :: check(:)
    allocate(check(size(matrix)))
    check = 0d0
    call assert_equal("no passed: node="//trim(to_str(node))//", i="//trim(to_str(i))//", j="//trim(to_str(j)), matrix, check)
  end subroutine

end module