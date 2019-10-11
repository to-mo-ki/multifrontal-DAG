module action_m
  use test_util
  implicit none
  
  interface
    subroutine action_i(matrix)
      double precision, pointer, contiguous :: matrix(:)
    end subroutine
  end interface
contains

  subroutine a_set_zero(matrix)
    double precision, pointer, contiguous :: matrix(:)
    matrix = 0.0d0
  end subroutine

  subroutine a_set_mark(matrix)
    double precision, pointer, contiguous :: matrix(:)
    matrix = matrix + 1.0d0
  end subroutine

  subroutine a_check_mark(matrix)
    double precision, pointer, contiguous :: matrix(:)
    integer :: i
    call assert_equal("test", matrix, (/(1d0, i=1, size(matrix))/))
  end subroutine

  subroutine a_check_no_mark(matrix)
    double precision, pointer, contiguous :: matrix(:)
    integer :: i
    call assert_equal("test", matrix, (/(0d0, i=1, size(matrix))/))
  end subroutine

end module