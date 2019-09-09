program disjoint_set_test
  use disjoint_set_m
  use test_util
  implicit none

  type(disjoint_set_c) :: set

  set = create_disjoint_set(5)
  call set%link(1, 2)
  call assert_equal("link(1,2);find(1)", set%find(1), 2)
  call set%link(3, 1)
  call assert_equal("link(3,1);find(3)", set%find(3), 2)
  call set%link(5, 4)
  call set%link(4, 3)
  call assert_equal("link(5,4);link(4, 3);find(5)", set%find(5), 2)
  call assert_equal("find(4)", set%find(4), 2)

  
end program disjoint_set_test