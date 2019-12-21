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
  
end program integer_function_test