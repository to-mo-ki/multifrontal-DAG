program supernode_controller_test
  use controller_test_routines_m, parent_check_estimate => check_estimate
  use controller_test_routines_m, parent_check_ptr => check_ptr
  use matrix_controller_m
  use supernode_controller_m
  implicit none
  
  call check_estimate(3, 5, 5, 44)
  call check_ptr(3, 5, 5, 1, 1, 1, 9)
  call check_ptr(3, 5, 5, 2, 1, 10, 9)
  call check_ptr(3, 5, 5, 4, 1, 28, 3)
  call check_ptr(3, 5, 5, 2, 2, 31, 6)
  call check_ptr(3, 5, 5, 3, 2, 37, 6)
  call check_ptr(3, 5, 5, 4, 2, 43, 2)

  call check_estimate(3, 6, 4, 51)
  call check_ptr(3, 6, 4, 2, 2, 31, 9)
  call check_ptr(3, 6, 4, 3, 2, 40, 9)
  call check_ptr(3, 6, 4, 4, 2, 49, 3)

  call check_estimate(3, 7, 4, 62)
  call check_ptr(3, 7, 4, 2, 1, 10, 9)
  call check_ptr(3, 7, 4, 4, 1, 28, 6)
  call check_ptr(3, 7, 4, 2, 2, 34, 9)
  call check_ptr(3, 7, 4, 3, 2, 43, 9)
  call check_ptr(3, 7, 4, 4, 2, 52, 6)
  call check_ptr(3, 7, 4, 3, 3, 58, 3)
  call check_ptr(3, 7, 4, 4, 3, 61, 2)

  call check_estimate(3, 5, 4, 39)
  call check_ptr(3, 5, 4, 2, 1, 10, 9)
  call check_ptr(3, 5, 4, 3, 1, 19, 9)
  call check_ptr(3, 5, 4, 2, 2, 28, 6)
  call check_ptr(3, 5, 4, 3, 2, 34, 6)

  call check_estimate(3, 3, 6, 27)
  call check_ptr(3, 3, 6, 2, 1, 10, 9)
  call check_ptr(3, 3, 6, 3, 1, 19, 9)

  call check_estimate(4, 2, 7, 18)
  call check_ptr(4, 2, 7, 1, 1, 1, 8)
  call check_ptr(4, 2, 7, 2, 1, 9, 8)
  call check_ptr(4, 2, 7, 3, 1, 17, 2)

contains
  
  subroutine check_estimate(nb, nc, nr, ans_size)
    integer, intent(in) :: nb, nc, nr
    integer, intent(in) :: ans_size
    class(matrix_controller_c), pointer :: controller
    
    allocate(supernode_controller_c::controller)
    call parent_check_estimate(controller, nb, nc, nr, ans_size)

  end subroutine

  subroutine check_ptr(nb, nc, nr, i, j, ans_pos, ans_size)
    integer, intent(in) :: nb, nc, nr, i, j
    integer, intent(in) :: ans_pos, ans_size
    class(matrix_controller_c), pointer :: controller
    
    allocate(supernode_controller_c::controller)
    call parent_check_ptr(controller, nb, nc, nr, i, j, ans_pos, ans_size)

  end subroutine
  
end program supernode_controller_test


