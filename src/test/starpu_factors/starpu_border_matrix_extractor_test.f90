program supernode_matrix_extractor_test
  use test_util
  use starpu_matrix_extractor_m
  use border_starpu_matrix_extractor_m
  use node_data_m
  implicit none
  class(extractor_c), pointer :: extractor
  integer :: array_size, i
  double precision, pointer, contiguous :: array(:)

  allocate(border_extractor_c::extractor)
  extractor%node_data => create_node_data([5,6,7,5,3],[5,4,4,4,6],3)
  call assert_equal("nb=3, nc=5, nr=5", extractor%estimate_size(1), 3)
  call assert_equal("nb=3, nc=6, nr=4", extractor%estimate_size(2), 0)
  call assert_equal("nb=3, nc=7, nr=4", extractor%estimate_size(3), 2)
  call assert_equal("nb=3, nc=5, nr=4", extractor%estimate_size(4), 2)
  call assert_equal("nb=3, nc=3, nr=6", extractor%estimate_size(5), 0)
  
  extractor%node_data => create_node_data([2,5],[7,2],4)
  call assert_equal("nb=4, nc=2, nr=7", extractor%estimate_size(1), 3)
  call assert_equal("nb=4, nc=5, nr=2(nr < nb)", extractor%estimate_size(2), 1)
  
end program supernode_matrix_extractor_test


