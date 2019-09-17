program heap_test
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
  call assert_equal("min_heap:non_empty", min_heap%is_empty(), .false.)
  do i=1, 9
    answer(i) = min_heap%get_top_node()
    call min_heap%delete_top_node()
  enddo
  call assert_equal("min_heap:empty", min_heap%is_empty(), .true.)
  call assert_equal("min_heap", answer, (/1, 2, 3, 7, 17, 19, 25, 36, 100/))

  max_heap => create_max_heap(10)
  call max_heap%build_heap(test)
  do i=1, 9
    answer(i) = max_heap%get_top_node()
    call max_heap%delete_top_node()
  enddo
  call assert_equal("max_heap", answer, (/100, 36, 25, 19, 17, 7, 3, 2, 1/))
  
  min_heap => create_min_heap(10)
  call min_heap%set_zero(9)
  do i=1, 9
    call min_heap%update_top_node(test(i))
  enddo
  call min_heap%update_top_node(2)
  call min_heap%update_top_node(12)
  call min_heap%update_top_node(102)
  do i=1, 9
    answer(i) = min_heap%get_top_node()
    call min_heap%delete_top_node()
  enddo
  call assert_equal("set_zero and add_top_node", answer, (/3, 7, 12, 17, 19, 25, 36, 100, 102/))
  
end program heap_test