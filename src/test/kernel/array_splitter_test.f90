program array_splitter_test
  use array_rearrange_kernel_m
  use test_util
  implicit none
  double precision, pointer, contiguous :: origin(:), dest(:), left(:), right(:)
  integer :: i

  allocate(origin, source=[(dble(i),i=1,7)])
  allocate(left(4))
  allocate(right(3))
  left = 1d0
  right = 1d0
  call split_array(origin, left, right, 4, 3)
  call assert_equal("left", left, [2d0,3d0,4d0,5d0])
  call assert_equal("right", right, [5d0,6d0,7d0])

  allocate(dest(7), source=0d0)
  left = [1,2,3,4]
  right = [5,6,7]
  call join_array(dest, left, right, 4, 3)
  call assert_equal("dest", dest, [1d0,2d0,3d0,4d0,5d0,6d0,7d0])
  
end program 