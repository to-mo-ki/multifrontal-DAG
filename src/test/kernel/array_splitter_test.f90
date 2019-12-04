program array_splitter_test
  use array_splitter_m
  use test_util
  implicit none
  double precision, pointer, contiguous :: origin(:), left(:), right(:)
  integer :: i

  allocate(origin, source=[(dble(i),i=1,7)])
  allocate(left(4))
  allocate(right(3))

  call split_array(origin, left, right, 4, 3)
  call assert_equal("left", left, [1d0,2d0,3d0,4d0])
  call assert_equal("right", right, [5d0,6d0,7d0])
  
end program 