program factors_size_estimator_test
  use factors_size_estimator_m
  use test_util
  implicit none

  call assert_equal("n=10, nb=3, nc=5, nr=5:supernode", estimate_supernode_size(5, 5, 3), 44)
  call assert_equal("n=10, nb=3, nc=5, nr=5:work", estimate_work_size(5, 5, 3), 18)
  call assert_equal("n=10, nb=3, nc=5, nr=5:border", estimate_border_size(5, 5, 3), 21)

  call assert_equal("n=10, nb=3, nc=6, nr=4:supernode", estimate_supernode_size(6, 4, 3), 51)
  call assert_equal("n=10, nb=3, nc=6, nr=4:work", estimate_work_size(6, 4, 3), 13)
  call assert_equal("n=10, nb=3, nc=6, nr=4:border", estimate_border_size(6, 4, 3), 0)

  call assert_equal("n=11, nb=3, nc=7, nr=4:supernode", estimate_supernode_size(7, 4, 3), 62)
  call assert_equal("n=11, nb=3, nc=7, nr=4:work", estimate_work_size(7, 4, 3), 12)
  call assert_equal("n=11, nb=3, nc=7, nr=4:border", estimate_border_size(7, 4, 3), 15)

  call assert_equal("n=9, nb=3, nc=5, nr=4:supernode", estimate_supernode_size(5, 4, 3), 39)
  call assert_equal("n=9, nb=3, nc=5, nr=4:work", estimate_work_size(5, 4, 3), 13)
  call assert_equal("n=9, nb=3, nc=5, nr=4:border", estimate_border_size(5, 4, 3), 18)

  call assert_equal("n=9, nb=3, nc=3, nr=6:supernode", estimate_supernode_size(3, 6, 3), 27)
  call assert_equal("n=9, nb=3, nc=3, nr=6:work", estimate_work_size(3, 6, 3), 27)
  call assert_equal("n=9, nb=3, nc=3, nr=6:border", estimate_border_size(3, 6, 3), 0)
  
end program factors_size_estimator_test