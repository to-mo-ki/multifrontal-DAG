program sort_test
  use sort_m
  use test_util
  implicit none
  integer :: i
  integer, allocatable :: a(:), perm(:)

  allocate(a(8), perm(8))
  a = [8, 4, 3, 7, 6, 5, 2, 1]
  call sort(a)
  call assert_equal("sort", a, [(i, i=1,8)])
  a = [8, 4, 3, 7, 6, 5, 2, 1]
  perm = [(i,i=1,8)]
  call sort_with_perm(a, perm)
  call assert_equal("sort_with_perm", a, [(i, i=1,8)])
  call assert_equal("sort_with_perm:perm", perm, [8,7,3,2,6,5,4,1])
  
end program sort_test