program block_size_calculator_test
  use block_size_calculator_m
  use test_util
  implicit none

  call assert_equal("index=1, nb=4, n=10", get_block_size(1, 4, 10), 4)
  call assert_equal("index=2, nb=4, n=10", get_block_size(2, 4, 10), 4)
  call assert_equal("index=3, nb=4, n=10", get_block_size(3, 4, 10), 2)
  
  call assert_equal("index=1, nb=4, n=10, offset=3", get_block_size(1, 4, 10, 3), 1)
  call assert_equal("index=2, nb=4, n=10, offset=3", get_block_size(2, 4, 10, 3), 4)
  call assert_equal("index=3, nb=4, n=10, offset=3", get_block_size(3, 4, 10, 3), 4)
  call assert_equal("index=4, nb=4, n=10, offset=3", get_block_size(4, 4, 10, 3), 1)
  
end program block_size_calculator_test