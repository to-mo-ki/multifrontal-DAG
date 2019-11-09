program block_size_calculator_test
  use block_size_calculator_m
  use test_util
  implicit none

  call assert_equal("index=1, nb=4, n=10", get_block_size(1, 4, 10), 4)
  call assert_equal("index=2, nb=4, n=10", get_block_size(2, 4, 10), 4)
  call assert_equal("index=3, nb=4, n=10", get_block_size(3, 4, 10), 2)
  
  call assert_equal("index=1, nb=4, n=10, first_block=1", get_block_size(1, 4, 10, 1), 1)
  call assert_equal("index=2, nb=4, n=10, first_block=1", get_block_size(2, 4, 10, 1), 4)
  call assert_equal("index=3, nb=4, n=10, first_block=1", get_block_size(3, 4, 10, 1), 4)
  call assert_equal("index=4, nb=4, n=10, first_block=1", get_block_size(4, 4, 10, 1), 1)

  call assert_equal("index=1, nb=4, n=10, first_block=0", get_block_size(1, 4, 10, 0), 4)
  call assert_equal("index=2, nb=4, n=10, first_block=0", get_block_size(2, 4, 10, 0), 4)
  call assert_equal("index=3, nb=4, n=10, first_block=0", get_block_size(3, 4, 10, 0), 2)
  
end program block_size_calculator_test