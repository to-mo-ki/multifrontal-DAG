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
  factors => create_factors(node_data, 5)
  a22 => factors%get_matrix_ptr(1,2,2)
  a32 => factors%get_matrix_ptr(1,3,2)
  a42 => factors%get_matrix_ptr(1,4,2)
  a22 = [double precision::(i,i=1,25)]
  a32 = [double precision::(i+25,i=1,25)]
  a42 = [double precision::(i+50,i=1,10)]

  call diag_test()
  call ndiag_test1()
  call ndiag_test2()

contains

  subroutine diag_test()
    call rearrange_diag(node_data, factors, 1, 2)

    allocate(chk, source=[double precision::1,6,7,11,12,13,16,17,18,21,22,23])
    allocate(pos, source=[1,4,5,7,8,9,10,11,12,13,14,15])
    ans => factors%get_supernode_ptr(1,2,2)
    call assert_equal_partial_array("diag_supernode", ans, pos, 12, chk)

    allocate(chk, source=[double precision::19,24,25])
    allocate(pos, source=[1,3,4])
    ans => factors%get_work_ptr(1,2,2)
    call assert_equal_partial_array("diag_work", ans, pos, 3, chk)

  end subroutine

  subroutine ndiag_test1()
    call rearrange_ndiag(node_data, factors, 1, 3, 2)

    allocate(chk, source=[double precision::26,27,28,31,32,33,36,37,38,41,42,43,46,47,48])
    ans => factors%get_supernode_ptr(1,3,2)
    call assert_equal("ndiag1_supernode", ans, chk)

    allocate(chk, source=[double precision::29,30,34,35,39,40,44,45,49,50])
    ans => factors%get_work_ptr(1,3,2)
    call assert_equal("ndiag1_work", ans, chk)

  end subroutine

  subroutine ndiag_test2()
    call rearrange_ndiag(node_data, factors, 1, 4, 2)

    allocate(chk, source=[double precision::51,52,53,56,57,58])
    ans => factors%get_supernode_ptr(1,4,2)
    call assert_equal("ndiag2_supernode", ans, chk)

    allocate(chk, source=[double precision::54,55,59,60])
    ans => factors%get_work_ptr(1,4,2)
    call assert_equal("ndiag2_work", ans, chk)

  end subroutine
  
end program rearrange_subroutines_test