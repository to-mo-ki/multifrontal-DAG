program node_data_test
  use node_data_m
  use test_util
  implicit none
  integer, pointer, contiguous :: supernode_size(:), work_size(:)
  integer :: tmp_i(6)
  logical :: tmp_l(6)
  type(node_data_c), pointer :: node_data
  integer :: i

  allocate(supernode_size(6), work_size(6))
  supernode_size = [5, 6, 7, 5, 3, 6]
  work_size = [5, 4, 4, 4, 6, 0]

  node_data => create_node_data(supernode_size, work_size, 3)

  do i=1, 6
    tmp_l(i) = node_data%divisible(i)
  enddo
  call assert_equal("divisible", tmp_l, [.false., .true., .false., .false., .true., .true.])
  
  do i=1, 6
    tmp_i(i) = node_data%get_border_supernode_size(i)
  enddo
  call assert_equal("border_supernode_size", tmp_i, [2, 0, 1, 2, 0, 0])

  do i=1, 6
    tmp_i(i) = node_data%get_border_work_size(i)
  enddo
  call assert_equal("border_work_size", tmp_i, [1, 3, 2, 1, 3, 3])

  do i=1, 6
    tmp_i(i) = node_data%get_num_supernode_block(i)
  enddo
  call assert_equal("num_supernode_block", tmp_i, [2, 2, 3, 2, 1, 2])

  do i=1, 6
    tmp_i(i) = node_data%get_num_work_block(i)
  enddo
  call assert_equal("num_work_block", tmp_i, [3, 2, 2, 2, 2, 0])

  call start_tests("get_work_size")
  call add_test("node=1, i=1", node_data%get_work_size(1,1), 1)
  call add_test("node=1, i=2", node_data%get_work_size(2,1), 3)
  call add_test("node=1, i=3", node_data%get_work_size(3,1), 1)
  call add_test("node=2, i=1", node_data%get_work_size(1,2), 3)
  call add_test("node=2, i=2", node_data%get_work_size(2,2), 1)
  call add_test("node=3, i=1", node_data%get_work_size(1,3), 2)
  call add_test("node=3, i=2", node_data%get_work_size(2,3), 2)
  call add_test("node=4, i=1", node_data%get_work_size(1,4), 1)
  call add_test("node=4, i=2", node_data%get_work_size(2,4), 3)
  call add_test("node=5, i=1", node_data%get_work_size(1,5), 3)
  call add_test("node=5, i=2", node_data%get_work_size(2,5), 3)
  call end_tests()


  call start_tests("get_matrix_num")
  call add_test("node=1, i=1", node_data%get_matrix_num(1), 1)
  call add_test("node=1, i=2", node_data%get_matrix_num(2), 1)
  call add_test("node=1, i=3", node_data%get_matrix_num(3), 1)
  call add_test("node=1, i=4", node_data%get_matrix_num(4), 2)
  call add_test("node=1, i=5", node_data%get_matrix_num(5), 2)
  call add_test("node=1, i=6", node_data%get_matrix_num(6), 2)
  call add_test("node=1, i=7", node_data%get_matrix_num(7), 3)
  call add_test("node=1, i=8", node_data%get_matrix_num(8), 3)
  call add_test("node=1, i=9", node_data%get_matrix_num(9), 3)
  call add_test("node=1, i=10", node_data%get_matrix_num(10), 4)
  call end_tests()

  call start_tests("get_work_num")
  call add_test("node=1, i=1", node_data%get_work_num(1,1), 2)
  call add_test("node=1, i=2", node_data%get_work_num(2,1), 3)
  call add_test("node=1, i=3", node_data%get_work_num(3,1), 3)
  call add_test("node=1, i=4", node_data%get_work_num(4,1), 3)
  call add_test("node=1, i=5", node_data%get_work_num(5,1), 4)
  call end_tests()

end program node_data_test