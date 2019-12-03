program border_solve_kernel_test
  use border_solve_kernel_m
  use test_util
  implicit none
  double precision :: matrix(3*5), rh(5)

  matrix = [4,0,0,3,3,0,2,2,2,1,1,1,2,2,2]
  rh = [4,6,6,3,6]
  call border_dtrsv_l(matrix, rh, 3, 2)
  call assert_equal("border_forward", rh, [1d0,1d0,1d0,0d0,0d0])

  matrix = [2,0,0,2,3,0,2,3,4,1,2,3,1,2,3]
  rh = [9,12,13,1,2]
  call border_dtrsv_u(matrix, rh, 3, 2)
  call assert_equal("border_backward", rh, [1d0,1d0,1d0,1d0,2d0])
  
end program 