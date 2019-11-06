program kernel_test
  use solve_kernel_m
  use test_util
  implicit none
  call dtrsv_l_test(3)
  call dtrsv_u_test(3)
  call dgemv_t_test
  call dgemv_n_test
contains

  subroutine dtrsv_l_test(n)
    ! [1      ] [x]   [1] 
    ! [1  1   ] [y] = [2]
    ! [1  1  1] [z]   [3]
    integer, intent(in) :: n
    integer :: i, j
    double precision :: a(n*n), b(n), check(n)
    do i=1,n
      do j=i,n
        a((j-1)*n+i) = 1d0
      enddo
    enddo
    b = (/ (dble(i),i=1,n) /)
    check = 1d0
    call mydtrsv_l(a, n, b)
    call assert_equal("dtrsv_l", b, check)

  end subroutine

  subroutine dtrsv_u_test(n)
    ! [1  1  1] [x]   [3] 
    ! [   1  1] [y] = [2]
    ! [      1] [z]   [1]
    integer, intent(in) :: n
    integer :: i, j
    double precision :: a(n*n), b(n), check(n)

    do i=1,n
      do j=i,n
        a((j-1)*n+i) = 1d0
      enddo
    enddo
    b = (/ (dble(n-i+1),i=1,n) /)
    check = 1d0
    call mydtrsv_u(a, n, b)
    call assert_equal("dtrsv_u", b, check)

  end subroutine

  subroutine dgemv_t_test
    ! [1  2  3] [1]   [14] 
    ! [4  5  6] [2] = [32]
    !           [3]
    integer :: i
    double precision :: a(3*2), x(3), y(2), check(2)

    a = (/ (dble(i),i=1,6) /)
    x(1)=1d0; x(2)=2d0; x(3)=3d0
    y = 1d0
    check = (/ -13d0, -31d0/)
    call mydgemv_t(a, 3, 2, x, y)
    call assert_equal("dgemv_t", y, check)
  end subroutine

  subroutine dgemv_n_test
    ! [1 4] [1]   [ 9] 
    ! [2 5] [2] = [12]
    ! [3 6]       [15]
    integer :: i
    double precision :: a(3*2), x(2), y(3), check(3)

    a = (/ (dble(i),i=1,6) /)
    x = (/ 1d0, 2d0/)
    y = 1d0
    check = (/ -8d0, -11d0, -14d0/)
    call mydgemv_n(a, 3, 2, x, y)
    call assert_equal("dgemv_n", y, check)
  end subroutine
end program kernel_test