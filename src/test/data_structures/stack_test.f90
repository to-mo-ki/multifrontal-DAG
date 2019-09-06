program stack_test
  use stack_m
  use test_util
  implicit none
  
  type(stack_c) :: stack

  stack = create_stack(10)
  call assert_equal("first empty", stack%is_empty(), .true.)
  call stack%push(1)
  call assert_equal("second empty", stack%is_empty(), .false.)
  call stack%push(2)
  call assert_equal("first pop", stack%pop(), 2)
  call assert_equal("second pop", stack%pop(), 1)
  call assert_equal("third empty", stack%is_empty(), .true.)

end program stack_test