program sparse_add_kernel_test
  use sparse_add_kernel_m
  use test_util
  implicit none
  double precision, pointer, contiguous :: x(:), y(:)
  integer, pointer, contiguous :: indx(:)
  integer :: i

  call scatter_test1
  call scatter_test2
  call gather_test1
  call gather_test2

contains
  subroutine scatter_test1()
    allocate(x, source=[1d0,2d0,3d0,4d0])
    allocate(y(8), source=0d0)
    allocate(indx, source=[2,3,7,8])

    call scatter_add_kernel(x, indx, y, 4)
    call assert_equal("scatter_test1", y, [0d0,1d0,2d0,0d0,0d0,0d0,3d0,4d0])

  end subroutine

  subroutine scatter_test2()
    allocate(x, source=[1d0,2d0,3d0,4d0,5d0,6d0])
    allocate(y(9), source=0d0)
    allocate(indx, source=[1,3,6,8,9])

    call scatter_add_kernel(x, indx, y, 5)
    call assert_equal("scatter_test2", y, [1d0,0d0,2d0,0d0,0d0,3d0,0d0,4d0,5d0])

  end subroutine

  subroutine gather_test1()
    allocate(x, source=[(dble(i),i=1,8)])
    allocate(y(4), source=0d0)
    allocate(indx, source=[2,3,7,8])

    call gather_add_kernel(x,indx, y, 4)
    call assert_equal("gather_test1", y, [2d0,3d0,7d0,8d0])

  end subroutine

  subroutine gather_test2()
    allocate(x, source=[(dble(i),i=1,9)])
    allocate(y(5), source=0d0)
    allocate(indx, source=[1,3,6,8,9])

    call gather_add_kernel(x, indx, y, 5)
    call assert_equal("gather_test2", y, [1d0,3d0,6d0,8d0,9d0])

  end subroutine

end program