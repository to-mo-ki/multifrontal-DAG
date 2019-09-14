program extend_test
  use extend_m
  use test_util
  implicit none

  class(super_c), pointer :: super
  integer :: i
  
  allocate(sub1_c::super)
  i = super%super_method()
  call assert_equal("extend sub1 test:1", i, 1)
  i = super%super_method()
  call assert_equal("extend sub1 test:2", i, 2)

  allocate(sub2_c::super)
  i = super%super_method()
  call assert_equal("extend sub2 test:1", i, 11)
  i = super%super_method()
  call assert_equal("extend sub2 test:2", i, 22)
  
end program extend_test