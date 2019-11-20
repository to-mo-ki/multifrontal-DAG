program factorize_kernel_test
  use factorize_kernel_m
  use test_util
  implicit none
  call dpotrf_test
  call dtrsm_test
  call dsyrk_test
  call dgemm_test
contains

  subroutine dpotrf_test()
    !  1   2   3      1
    !  2   8  12  =>  2  2
    !  3  12  27      3  3  3
    double precision :: a(9), check(6)
    integer :: pos(6), info

    a(1) = 1; a(4) = 2; a(5) = 8
    a(7) = 3; a(8) = 12; a(9) = 27

    pos = (/1, 4, 5, 7, 8, 9/)
    check = (/1d0, 2d0, 2d0, 3d0, 3d0, 3d0/)

    call mydpotrf(3, a, info)
    call assert_equal_partial_array("dpotrf test", a, pos, 6, check)


  end subroutine

  subroutine dtrsm_test
    !  [1      ] [1  4]     [ 1   4]
    !  [2  2   ] [2  5]  =  [ 6  18]
    !  [3  3  3] [3  6]     [18  45]
    double precision :: a(9), b(6), check(6)

    a(1) = 1; a(4) = 2; a(5) = 2
    a(7) = 3; a(8) = 3; a(9) = 3
    b = (/1d0, 6d0, 18d0, 4d0, 18d0, 45d0/)
    check = (/1d0, 2d0, 3d0, 4d0, 5d0, 6d0/)

    call mydtrsm(3, 2, a, b)
    call assert_equal("dtrsm", b, check)

  end subroutine

  subroutine dsyrk_test()
    !  [1  2  3] [1  4]     [14   32]
    !  [4  5  6] [2  5]  =  [32   77]
    !            [3  6]
    double precision :: a(6), b(4), check(3)
    integer :: i, pos(3)

    a = (/ (dble(i),i=1,6) /)
    b = 1.0d0
    call mydsyrk(2, 3, a, b)
    check = (/-13d0, -31d0, -76d0/)
    pos = (/1, 3, 4/)
    call assert_equal_partial_array("dsyrk", b, pos, 3, check)

  end subroutine

  subroutine dgemm_test()
    !         [ 1  4  7  10]
    ! [1 2 3] [ 2  5  8  11] = [14 32  50  68]
    ! [4 5 6] [ 3  6  9  12]   [32 76 122 166]
    ! <=>
    ! [ 1  2  3] [1 4]   [14  32]
    ! [ 4  5  6] [2 5] = [32  77]
    ! [ 7  8  9] [3 6]   [50 122]
    ! [10 11 12]         [68 167]
    double precision :: lower(6), upper(12), update(8), check(8)
    integer :: i

    upper = (/ (dble(i),i=1,12) /)
    lower = (/ (dble(i),i=1,6) /)
    update = 1.0d0
    call mydgemm(3, 2, 4, lower, upper, update)
    check = (/-13d0, -31d0, -49d0, -67d0, -31d0, -76d0, -121d0, -166d0/)
    call assert_equal("dgemm", update, check)

  end subroutine
end program