program sort_test
  use sort_m
  use test_util
  implicit none
  integer :: i
  integer, allocatable :: a(:)

  allocate(a(8))
  a = [8, 4, 3, 7, 6, 5, 2, 1]
  call sort(a, 8)
  call assert_equal("sort", a, [(i, i=1,8)])
  
end program sort_test