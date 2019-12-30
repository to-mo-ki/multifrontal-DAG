program rearrange_subroutines_test
  use factors_m
  use node_data_m
  use rearrange_subroutines_m
  use test_util
  implicit none
  type(factors_c), pointer :: factors
  type(node_data_c), pointer :: node_data
  double precision, pointer, contiguous :: a22(:), a32(:), a42(:), ans(:), chk(:)
  integer, pointer, contiguous :: pos(:)
  integer :: i

  node_data => create_node_data([8,9],[9,0], 5)
  factors => create_factors(node_data)
  a22 => factors%get_matrix(1,2,2)
  a32 => factors%get_matrix(1,3,2)
  a42 => factors%get_matrix(1,4,2)
  a22 = [(dble(i),i=1,25)]
  a32 = [(dble(i+25),i=1,25)]
  a42 = [(dble(i+50),i=1,10)]

  call diag_test()
  call ndiag_test1()
  call ndiag_test2()

contains

  subroutine diag_test()
    call rearrange_diag(node_data, factors, 1, 2)

    allocate(chk, source=[1d0,6d0,7d0,11d0,12d0,13d0,16d0,17d0,18d0,21d0,22d0,23d0])
    allocate(pos, source=[1,4,5,7,8,9,10,11,12,13,14,15])
    ans => factors%get_supernode(1,2,2)
    call assert_equal_partial_array("diag_supernode", ans, pos, 12, chk)

    allocate(chk, source=[19d0,24d0,25d0])
    allocate(pos, source=[1,3,4])
    ans => factors%get_work(1,2,2)
    call assert_equal_partial_array("diag_work", ans, pos, 3, chk)

  end subroutine

  subroutine ndiag_test1()
    call rearrange_ndiag(node_data, factors, 1, 3, 2)

    allocate(chk, source=[26d0,27d0,28d0,31d0,32d0,33d0,36d0,37d0,38d0,41d0,42d0,43d0,46d0,47d0,48d0])
    ans => factors%get_supernode(1,3,2)
    call assert_equal("ndiag1_supernode", ans, chk)

    allocate(chk, source=[29d0,30d0,34d0,35d0,39d0,40d0,44d0,45d0,49d0,50d0])
    ans => factors%get_work(1,3,2)
    call assert_equal("ndiag1_work", ans, chk)

  end subroutine

  subroutine ndiag_test2()
    call rearrange_ndiag(node_data, factors, 1, 4, 2)

    allocate(chk, source=[51d0,52d0,53d0,56d0,57d0,58d0])
    ans => factors%get_supernode(1,4,2)
    call assert_equal("ndiag2_supernode", ans, chk)

    allocate(chk, source=[54d0,55d0,59d0,60d0])
    ans => factors%get_work(1,4,2)
    call assert_equal("ndiag2_work", ans, chk)

  end subroutine
  
end program rearrange_subroutines_test