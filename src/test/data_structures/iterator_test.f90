program iterator_test
  use iterator_m
  use test_util
  implicit none
  integer, pointer, contiguous :: next_node(:)
  type(iterator_c), pointer :: iterator

  allocate(next_node(5))

  next_node = [3, 5, 0, 2, 1]
  iterator => create_iterator(4, next_node)
  call start_tests("has_next & next")
  call add_test("has_next(1)", iterator%has_next(), .true.)
  call add_test("next(1)", iterator%next(), 4)
  call add_test("has_next(2)", iterator%has_next(), .true.)
  call add_test("next(2)", iterator%next(), 2)
  call add_test("has_next(3)", iterator%has_next(), .true.)
  call add_test("next(3)", iterator%next(), 5)
  call add_test("has_next(4)", iterator%has_next(), .true.)
  call add_test("next(4)", iterator%next(), 1)
  call add_test("has_next(5)", iterator%has_next(), .true.)
  call add_test("next(5)", iterator%next(), 3)
  call add_test("has_next(6)", iterator%has_next(), .false.)
  call end_tests()
  
end program iterator_test