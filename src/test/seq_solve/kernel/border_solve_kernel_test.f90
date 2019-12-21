program border_solve_kernel_test
  use border_solve_kernel_m
  use test_util
  implicit none
  double precision :: matrix(3*5), rh1(3), rh2(2)

  matrix = [4,0,0,3,3,0,2,2,2,1,1,1,2,2,2]
  rh1 = [4,6,6]
  rh2 = [3,6]
  call border_dtrsv_l(matrix, rh1, rh2, 3, 2)
  call assert_equal("border_forward", [rh1, rh2], [1d0,1d0,1d0,0d0,0d0])

  matrix = [2,0,0,2,3,0,2,3,4,1,2,3,1,2,3]
  rh1 = [9,12,13]
  rh2 = [1,2]
  call border_dtrsv_u(matrix, rh1, rh2, 3, 2)
  call assert_equal("border_backward", [rh1, rh2], [1d0,1d0,1d0,1d0,2d0])
  
end program 