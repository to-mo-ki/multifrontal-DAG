program factors_pos_calculator_test
  use factors_pos_calculator_m
  use test_util
  implicit none
  
  call check_supernode(3, 5, 5, 1, 1, 1, 9)
  call check_supernode(3, 5, 5, 2, 1, 10, 9)
  call check_supernode(3, 5, 5, 4, 1, 28, 3)
  call check_supernode(3, 5, 5, 2, 2, 31, 6)
  call check_supernode(3, 5, 5, 3, 2, 37, 6)
  call check_supernode(3, 5, 5, 4, 2, 43, 2)
  
  call check_work(3, 5, 5, 2, 2, 1, 1)
  call check_work(3, 5, 5, 3, 2, 2, 3)
  call check_work(3, 5, 5, 4, 2, 5, 1)
  call check_work(3, 5, 5, 3, 3, 6, 9)
  call check_work(3, 5, 5, 4, 3, 15, 3)
  call check_work(3, 5, 5, 4, 4, 18, 1)

  call check_border(3, 5, 5, 2, 2, 1, 9)
  call check_border(3, 5, 5, 3, 2, 10, 9)
  call check_border(3, 5, 5, 4, 2, 19, 3)

  call check_supernode(3, 6, 4, 2, 2, 31, 9)
  call check_supernode(3, 6, 4, 3, 2, 40, 9)
  call check_supernode(3, 6, 4, 4, 2, 49, 3)

  call check_work(3, 6, 4, 3, 3, 1, 9)
  call check_work(3, 6, 4, 4, 3, 10, 3)
  call check_work(3, 6, 4, 4, 4, 13, 1)

  call check_supernode(3, 7, 4, 2, 1, 10, 9)
  call check_supernode(3, 7, 4, 4, 1, 28, 6)
  call check_supernode(3, 7, 4, 2, 2, 34, 9)
  call check_supernode(3, 7, 4, 3, 2, 43, 9)
  call check_supernode(3, 7, 4, 4, 2, 52, 6)
  call check_supernode(3, 7, 4, 3, 3, 58, 3)
  call check_supernode(3, 7, 4, 4, 3, 61, 2)

  call check_work(3, 7, 4, 3, 3, 1, 4)
  call check_work(3, 7, 4, 4, 3, 5, 4)
  call check_work(3, 7, 4, 4, 4, 9, 4)
  
  call check_border(3, 7, 4, 3, 3, 1, 9)
  call check_border(3, 7, 4, 4, 3, 10, 6)

  call check_supernode(3, 5, 4, 2, 1, 10, 9)
  call check_supernode(3, 5, 4, 3, 1, 19, 9)
  call check_supernode(3, 5, 4, 2, 2, 28, 6)
  call check_supernode(3, 5, 4, 3, 2, 34, 6)

  call check_work(3, 5, 4, 2, 2, 1, 1)
  call check_work(3, 5, 4, 3, 2, 2, 3)
  call check_work(3, 5, 4, 3, 3, 5, 9)

  call check_border(3, 5, 4, 2, 2, 1, 9)
  call check_border(3, 5, 4, 3, 2, 10, 9)

  call check_supernode(3, 3, 6, 2, 1, 10, 9)
  call check_supernode(3, 3, 6, 3, 1, 19, 9)
  
  call check_work(3, 3, 6, 2, 2, 1, 9)
  call check_work(3, 3, 6, 3, 2, 10, 9)
  call check_work(3, 3, 6, 3, 3, 19, 9)

  call check_supernode(4, 2, 7, 1, 1, 1, 8)
  call check_supernode(4, 2, 7, 2, 1, 9, 8)
  call check_supernode(4, 2, 7, 3, 1, 17, 2)
  
  call check_work(4, 2, 7, 1, 1, 1, 4)
  call check_work(4, 2, 7, 2, 1, 5, 8)
  call check_work(4, 2, 7, 3, 1, 13, 2)
  call check_work(4, 2, 7, 2, 2, 15, 16)
  call check_work(4, 2, 7, 3, 2, 31, 4)
  call check_work(4, 2, 7, 3, 3, 35, 1)

  call check_border(4, 2, 7, 1, 1, 1, 16)
  call check_border(4, 2, 7, 2, 1, 17, 16)
  call check_border(4, 2, 7, 3, 1, 33, 4)
  
  
  !call assert_equal("n=10, nb=3, nc=5, nr=5, i=1, j=1:work_pos", estimate_work_size(5, 5, 3), 18)
  !call assert_equal("n=10, nb=3, nc=5, nr=5, i=1, j=1:border_pos", estimate_border_size(5, 5, 3), 21)
  
end program factors_pos_calculator_test

subroutine check_supernode(nb, nc, nr, i, j, ans_pos, ans_size)
  use factors_pos_calculator_m
  use test_util
  !TODO: 一時的, test_utilに含めたい
  use to_str_m
  integer, intent(in) :: nb, nc, nr, i, j
  integer, intent(in) :: ans_pos, ans_size
  integer :: n
  
  n = nc + nr
  call assert_equal("n="//trim(to_str(n))//&
    &", nb="//trim(to_str(nb))//&
    &", nc="//trim(to_str(nc))//&
    &", nr="//trim(to_str(nr))//&
    &", i="//trim(to_str(i))//&
    &", j="//trim(to_str(j))//&
    &":supernode_pos", get_supernode_pos(nb, nc, nr, i, j), ans_pos)

  call assert_equal("n="//trim(to_str(n))//&
    &", nb="//trim(to_str(nb))//&
    &", nc="//trim(to_str(nc))//&
    &", nr="//trim(to_str(nr))//&
    &", i="//trim(to_str(i))//&
    &", j="//trim(to_str(j))//&
    &":supernode_size", get_supernode_size(nb, nc, nr, i, j), ans_size)
  
end subroutine

subroutine check_work(nb, nc, nr, i, j, ans_pos, ans_size)
  use factors_pos_calculator_m
  use test_util
  use to_str_m
  integer, intent(in) :: nb, nc, nr, i, j
  integer, intent(in) :: ans_pos, ans_size
  integer :: n
  
  n = nc + nr
  call assert_equal("n="//trim(to_str(n))//&
    &", nb="//trim(to_str(nb))//&
    &", nc="//trim(to_str(nc))//&
    &", nr="//trim(to_str(nr))//&
    &", i="//trim(to_str(i))//&
    &", j="//trim(to_str(j))//&
    &":work_pos", get_work_pos(nb, nc, nr, i, j), ans_pos)

  call assert_equal("n="//trim(to_str(n))//&
    &", nb="//trim(to_str(nb))//&
    &", nc="//trim(to_str(nc))//&
    &", nr="//trim(to_str(nr))//&
    &", i="//trim(to_str(i))//&
    &", j="//trim(to_str(j))//&
    &":work_size", get_work_size(nb, nc, nr, i, j), ans_size)

end subroutine

subroutine check_border(nb, nc, nr, i, j, ans_pos, ans_size)
  use factors_pos_calculator_m
  use test_util
  use to_str_m
  integer, intent(in) :: nb, nc, nr, i, j
  integer, intent(in) :: ans_pos, ans_size
  integer :: n
  
  n = nc + nr
  
  call assert_equal("n="//trim(to_str(n))//&
    &", nb="//trim(to_str(nb))//&
    &", nc="//trim(to_str(nc))//&
    &", nr="//trim(to_str(nr))//&
    &", i="//trim(to_str(i))//&
    &", j="//trim(to_str(j))//&
    &":border_pos", get_border_pos(nb, nc, nr, i, j), ans_pos)

  call assert_equal("n="//trim(to_str(n))//&
    &", nb="//trim(to_str(nb))//&
    &", nc="//trim(to_str(nc))//&
    &", nr="//trim(to_str(nr))//&
    &", i="//trim(to_str(i))//&
    &", j="//trim(to_str(j))//&
    &":border_size", get_border_size(nb, nc, nr, i, j), ans_size)

end subroutine
