program integer_function_test
  use integer_function_m
  use test_util
  implicit none

  call assert_equal("1 to 4", partial_sum(4), 10)
  call assert_equal("1 to 0", partial_sum(0), 0)
  call assert_equal("1 to 6", partial_sum(6), 21)
  call assert_equal("2 to 6", partial_sum(2, 6), 20)
  call assert_equal("3 to 6", partial_sum(3, 6), 18)
  call assert_equal("1 to -1", partial_sum(1, -1), 0)
  call assert_equal("1 to 0", partial_sum(1, 0), 0)
  call assert_equal("2 to 1", partial_sum(2, 1), 0)
  call assert_equal("3 to 2", partial_sum(3, 2), 0)
  call assert_equal("3 to 1", partial_sum(3, 1), 0)
  call assert_equal("5 to 5", partial_sum(5, 5), 5)

  print *, "triangular_test"
  call assert_equal("n=4, (i, j) = (1, 1)", triangular_pos(1,1,4), 1)
  call assert_equal("n=4, (i, j) = (2, 1)", triangular_pos(2,1,4), 2)
  call assert_equal("n=4, (i, j) = (3, 1)", triangular_pos(3,1,4), 3)
  call assert_equal("n=4, (i, j) = (4, 1)", triangular_pos(4,1,4), 4)
  call assert_equal("n=4, (i, j) = (2, 2)", triangular_pos(2,2,4), 5)
  call assert_equal("n=4, (i, j) = (3, 2)", triangular_pos(3,2,4), 6)
  call assert_equal("n=4, (i, j) = (4, 2)", triangular_pos(4,2,4), 7)
  call assert_equal("n=4, (i, j) = (3, 3)", triangular_pos(3,3,4), 8)
  call assert_equal("n=4, (i, j) = (4, 3)", triangular_pos(4,3,4), 9)
  call assert_equal("n=4, (i, j) = (4, 4)", triangular_pos(4,4,4), 10)
  
end program integer_function_test