module controller_test_routines_m
  use matrix_controller_m
  !TODO: 一時措置
  use to_str_m
  use test_util
  implicit none
  private

  public :: check_ptr, check_estimate
  
contains

  subroutine check_ptr(controller, nb, nc, nr, i, j, ans_pos, ans_size)
    integer, intent(in) :: nb, nc, nr, i, j
    integer, intent(in) :: ans_pos, ans_size
    class(matrix_controller_c), pointer :: controller
    double precision, pointer :: array(:)
    double precision, pointer, contiguous :: matrix(:)
    integer :: n, array_size

    array_size = controller%estimate_size(nb, nc, nr)
    allocate(array(array_size))
    array = (/(dble(i), i=1,array_size)/)
    n = nc + nr
    matrix => controller%get_ptr(array, nb, nc, nr, i, j)

    call assert_equal("n="//to_str(n)//&
      &", nb="//to_str(nb)//&
      &", nc="//to_str(nc)//&
      &", nr="//to_str(nr)//&
      &", i="//to_str(i)//&
      &", j="//to_str(j)//&
      &":pos", nint(matrix(1)), ans_pos)
    
    call assert_equal("n="//to_str(n)//&
      &", nb="//to_str(nb)//&
      &", nc="//to_str(nc)//&
      &", nr="//to_str(nr)//&
      &", i="//to_str(i)//&
      &", j="//to_str(j)//&
      &":size", size(matrix), ans_size)

    deallocate(array)

  end subroutine

  subroutine check_estimate(controller, nb, nc, nr, ans_size)
    integer, intent(in) :: nb, nc, nr
    integer, intent(in) :: ans_size
    class(matrix_controller_c), pointer :: controller
    integer :: n

    n = nr + nc

    call assert_equal("n="//to_str(n)//&
      &", nb="//to_str(nb)//&
      &", nc="//to_str(nc)//&
      &", nr="//to_str(nr)//&
      &":size", controller%estimate_size(nb, nc, nr), ans_size)

  end subroutine
end module