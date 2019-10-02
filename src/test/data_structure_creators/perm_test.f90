program perm_test
  use perm_m
  use test_util
  implicit none
  
  integer, pointer, contiguous :: perm(:), iperm(:)
  integer, pointer, contiguous :: in_int(:), out_int(:)
  double precision, pointer, contiguous :: in_DP(:), out_DP(:)
  integer :: n, i

  n = 9
  allocate(perm(n))
  allocate(in_int(n))
  allocate(in_DP(n))
  perm = (/1, 7, 2, 4, 3, 5, 6, 8, 9/)
  
  in_int = (/(i, i=1,9)/)
  call permutate(perm, in_int, out_int)
  call assert_equal("permtate_int", out_int, (/1, 7, 2, 4, 3, 5, 6, 8, 9/))

  in_DP = (/(dble(i), i=1,9)/)
  call permutate(perm, in_DP, out_DP)
  call assert_equal("permtate_DP", out_DP, (/1d0, 7d0, 2d0, 4d0, 3d0, 5d0, 6d0, 8d0, 9d0/))
  
  call inverse_permutate(perm, in_DP, out_DP)
  call assert_equal("inverse_permutate", out_DP, (/1d0, 3d0, 5d0, 4d0, 6d0, 7d0, 2d0, 8d0, 9d0/))

  call set_iperm(perm, iperm)
  call assert_equal("set_iperm", iperm, (/1, 3, 5, 4, 6, 7, 2, 8, 9/))

end program perm_test