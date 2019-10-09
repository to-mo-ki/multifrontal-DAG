program supernode_controller_test
  use controller_test_routines_m, parent_check_estimate => check_estimate
  use controller_test_routines_m, parent_check_ptr => check_ptr
  use matrix_controller_m
  use border_controller_m
  implicit none

  call check_estimate(3, 5, 5, 21)
  call check_ptr(3, 5, 5, 2, 2, 1, 9)
  call check_ptr(3, 5, 5, 3, 2, 10, 9)
  call check_ptr(3, 5, 5, 4, 2, 19, 3)

  call check_estimate(3, 6, 4, 0)

  call check_estimate(3, 7, 4, 15)
  call check_ptr(3, 7, 4, 3, 3, 1, 9)
  call check_ptr(3, 7, 4, 4, 3, 10, 6)

  call check_estimate(3, 5, 4, 18)
  call check_ptr(3, 5, 4, 2, 2, 1, 9)
  call check_ptr(3, 5, 4, 3, 2, 10, 9)

  call check_estimate(3, 3, 6, 0)

  call check_estimate(4, 2, 7, 36)
  call check_ptr(4, 2, 7, 1, 1, 1, 16)
  call check_ptr(4, 2, 7, 2, 1, 17, 16)
  call check_ptr(4, 2, 7, 3, 1, 33, 4)
  
contains
  
  subroutine check_estimate(nb, nc, nr, ans_size)
    integer, intent(in) :: nb, nc, nr
    integer, intent(in) :: ans_size
    class(matrix_controller_c), pointer :: controller
    
    allocate(border_controller_c::controller)
    call parent_check_estimate(controller, nb, nc, nr, ans_size)

  end subroutine

  subroutine check_ptr(nb, nc, nr, i, j, ans_pos, ans_size)
    integer, intent(in) :: nb, nc, nr, i, j
    integer, intent(in) :: ans_pos, ans_size
    class(matrix_controller_c), pointer :: controller
    
    allocate(border_controller_c::controller)
    call parent_check_ptr(controller, nb, nc, nr, i, j, ans_pos, ans_size)

  end subroutine
  
end program supernode_controller_test


