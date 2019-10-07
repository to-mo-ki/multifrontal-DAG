program partial_sum_test
  use partial_sum_m
  use test_util
  implicit none

  call assert_equal("1 to 4", partial_sum(1, 4), 10)
  call assert_equal("1 to 0", partial_sum(1, 0), 0)
  call assert_equal("1 to 6", partial_sum(1, 6), 21)
  call assert_equal("2 to 6", partial_sum(2, 6), 20)
  call assert_equal("3 to 6", partial_sum(3, 6), 18)
  call assert_equal("1 to -1", partial_sum(1, -1), 0)
  
end program partial_sum_test