program starpu_work_matrix_extractor_test
  use test_util
  use starpu_matrix_extractor_m
  use work_starpu_matrix_extractor_m
  use node_data_m
  implicit none
  class(extractor_c), pointer :: extractor
  double precision, pointer, contiguous :: array(:)
  
  allocate(work_extractor_c::extractor)
  extractor%node_data => create_node_data([5,6,7,5,3,6],[5,4,4,4,6,0],3)

  call assert_equal("nb=3, nc=5, nr=5", extractor%estimate_size(1), 6)
  call assert_equal("nb=3, nc=6, nr=4", extractor%estimate_size(2), 3)
  call assert_equal("nb=3, nc=7, nr=4", extractor%estimate_size(3), 3)
  call assert_equal("nb=3, nc=5, nr=4", extractor%estimate_size(4), 3)
  call assert_equal("nb=3, nc=3, nr=6", extractor%estimate_size(5), 3)
  call assert_equal("nb=3, nc=6, nr=0", extractor%estimate_size(6), 0)

  extractor%node_data => create_node_data([2],[7],4)
  call assert_equal("nb=4, nc=2, nr=7", extractor%estimate_size(1), 6)
  
  extractor%node_data => create_node_data([1],[7],10)
  call assert_equal("nb=10, nc=1, nr=7", extractor%estimate_size(1), 1)
  
end program