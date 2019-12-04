program sparse_add_kernel_test
  use sparse_add_kernel_m
  use test_util
  implicit none
  double precision, pointer, contiguous :: x(:), y(:)
  integer, pointer, contiguous :: indx(:)

  call test1
  call test2

contains
  subroutine test1()
    allocate(x, source=[1d0,2d0,3d0,4d0])
    allocate(y(8), source=0d0)
    allocate(indx, source=[2,3,7,8])

    call myaxpyi(x, indx, y, 4)
    call assert_equal("test1", y, [0d0,-1d0,-2d0,0d0,0d0,0d0,-3d0,-4d0])

  end subroutine

  subroutine test2()
    allocate(x, source=[1d0,2d0,3d0,4d0,5d0,6d0])
    allocate(y(9), source=0d0)
    allocate(indx, source=[1,3,6,8,9])

    call myaxpyi(x, indx, y, 5)
    call assert_equal("test2", y, [-1d0,0d0,-2d0,0d0,0d0,-3d0,0d0,-4d0,-5d0])

  end subroutine

end program