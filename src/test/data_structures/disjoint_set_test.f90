program disjoint_set_test
  use disjoint_set_m
  use test_util
  implicit none

  type(disjoint_set_c), pointer :: set

  set => create_disjoint_set(5)
  call set%link(1, 2)
  call assert_equal("link(1,2);find(1)", set%find(1), 2)
  call set%link(3, 2)
  call assert_equal("link(3,2);find(3)", set%find(3), 2)
  call set%link(4, 5)
  call set%link(2, 5)
  call assert_equal("link(4,5);link(2,5);find(2)", set%find(2), 5)
  call assert_equal("find(3)", set%find(3), 5)

  
end program disjoint_set_test