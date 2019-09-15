program min_heap_test
  use heap_m
  use test_util
  implicit none
  type(heap_c), pointer :: min_heap, max_heap
  integer, pointer, contiguous :: test(:), answer(:)
  integer :: i

  allocate(test(9), answer(9))
  test = (/19, 36, 25, 1, 3, 17, 7, 2, 100/)

  min_heap => create_min_heap(10)
  call min_heap%build_heap(test)
  call assert_equal("min_heap:max", min_heap%max(), 100)
  do i=1, 9
    answer(i) = min_heap%pop_top_node()
  enddo
  call assert_equal("min_heap", answer, (/1, 2, 3, 7, 17, 19, 25, 36, 100/))

  max_heap => create_max_heap(10)
  call max_heap%build_heap(test)
  do i=1, 9
    answer(i) = max_heap%pop_top_node()
  enddo
  call assert_equal("max_heap", answer, (/100, 36, 25, 19, 17, 7, 3, 2, 1/))
  
  min_heap => create_min_heap(10)
  call min_heap%set_zero(9)
  do i=1, 9
    call min_heap%add_top_node(test(i))
  enddo
  call min_heap%add_top_node(1)
  call min_heap%add_top_node(10)
  call min_heap%add_top_node(100)
  do i=1, 9
    answer(i) = min_heap%pop_top_node()
  enddo
  call assert_equal("set_zero and add_top_node", answer, (/3, 7, 12, 17, 19, 25, 36, 100, 102/))
  
end program min_heap_test