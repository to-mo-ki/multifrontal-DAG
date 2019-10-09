program work_controller_test
  use controller_test_routines_m, parent_check_estimate => check_estimate
  use controller_test_routines_m, parent_check_ptr => check_ptr
  use matrix_controller_m
  use work_controller_m
  implicit none
  
  call check_estimate(3, 5, 5, 18)
  call check_ptr(3, 5, 5, 2, 2, 1, 1)
  call check_ptr(3, 5, 5, 3, 2, 2, 3)
  call check_ptr(3, 5, 5, 4, 2, 5, 1)
  call check_ptr(3, 5, 5, 3, 3, 6, 9)
  call check_ptr(3, 5, 5, 4, 3, 15, 3)
  call check_ptr(3, 5, 5, 4, 4, 18, 1)
  
  call check_estimate(3, 6, 4, 13)
  call check_ptr(3, 6, 4, 3, 3, 1, 9)
  call check_ptr(3, 6, 4, 4, 3, 10, 3)
  call check_ptr(3, 6, 4, 4, 4, 13, 1)

  call check_estimate(3, 7, 4, 12)
  call check_ptr(3, 7, 4, 3, 3, 1, 4)
  call check_ptr(3, 7, 4, 4, 3, 5, 4)
  call check_ptr(3, 7, 4, 4, 4, 9, 4)

  call check_estimate(3, 5, 4, 13)
  call check_ptr(3, 5, 4, 2, 2, 1, 1)
  call check_ptr(3, 5, 4, 3, 2, 2, 3)
  call check_ptr(3, 5, 4, 3, 3, 5, 9)

  call check_estimate(3, 3, 6, 27)
  call check_ptr(3, 3, 6, 2, 2, 1, 9)
  call check_ptr(3, 3, 6, 3, 2, 10, 9)
  call check_ptr(3, 3, 6, 3, 3, 19, 9)

  call check_estimate(4, 2, 7, 35)
  call check_ptr(4, 2, 7, 1, 1, 1, 4)
  call check_ptr(4, 2, 7, 2, 1, 5, 8)
  call check_ptr(4, 2, 7, 3, 1, 13, 2)
  call check_ptr(4, 2, 7, 2, 2, 15, 16)
  call check_ptr(4, 2, 7, 3, 2, 31, 4)
  call check_ptr(4, 2, 7, 3, 3, 35, 1)

contains
  
  subroutine check_estimate(nb, nc, nr, ans_size)
    integer, intent(in) :: nb, nc, nr
    integer, intent(in) :: ans_size
    class(matrix_controller_c), pointer :: controller
    
    allocate(work_controller_c::controller)
    call parent_check_estimate(controller, nb, nc, nr, ans_size)

  end subroutine

  subroutine check_ptr(nb, nc, nr, i, j, ans_pos, ans_size)
    integer, intent(in) :: nb, nc, nr, i, j
    integer, intent(in) :: ans_pos, ans_size
    class(matrix_controller_c), pointer :: controller
    
    allocate(work_controller_c::controller)
    call parent_check_ptr(controller, nb, nc, nr, i, j, ans_pos, ans_size)

  end subroutine
  
end program work_controller_test