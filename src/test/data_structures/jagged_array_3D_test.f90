program jagged_array_3D_test
  use jagged_array_3D_m
  use jagged_array_m
  use contiguous_sets_m
  use test_util
  implicit none
  type(contiguous_sets_c), pointer :: set
  type(jagged_array_c), pointer :: jag_2d
  type(jagged_array_3D_c), pointer :: jag_3d
  integer, pointer, contiguous :: jag_2d_lengths(:), jag_2d_val(:)

  set => create_contiguous_sets([4, 2, 3, 3, 0])
  allocate(jag_2d_lengths(12), jag_2d_val(28))
  jag_2d_lengths = [3, 3, 1, 2, 2, 2, 3, 3, 2, 3, 0, 4]
  jag_2d_val = [111, 112, 113, 121, 122, 123, 131, 141, 142, 211, 212, 221, 222, 311, 312, 313, 321, 322, 323, 331, 332, 411, 412, 413, 431, 432, 433, 434]
  jag_2d => create_jagged_array(jag_2d_lengths, jag_2d_val)
  jag_3d => create_jagged_array_3D(set, jag_2d)

  call start_array_tests("get_array")
  call add_test("(1, 1)", jag_3d%get_array(1, 1), [111, 112, 113])
  call add_test("(1, 2)", jag_3d%get_array(1, 2), [121, 122, 123])
  call add_test("(1, 3)", jag_3d%get_array(1, 3), [131])
  call add_test("(1, 4)", jag_3d%get_array(1, 4), [141, 142])
  call add_test("(2, 1)", jag_3d%get_array(2, 1), [211, 212])
  call add_test("(2, 2)", jag_3d%get_array(2, 2), [221, 222])
  call add_test("(3, 1)", jag_3d%get_array(3, 1), [311, 312, 313])
  call add_test("(3, 2)", jag_3d%get_array(3, 2), [321, 322, 323])
  call add_test("(3, 3)", jag_3d%get_array(3, 3), [331, 332])
  call add_test("(4, 1)", jag_3d%get_array(4, 1), [411, 412, 413])
  call add_test("(4, 3)", jag_3d%get_array(4, 3), [431, 432, 433, 434])
  call end_array_tests()

  call assert_equal("get_num_1d", jag_3d%get_num_1d(), 5)
  
  call start_tests("get_num_2d")
  call add_test("1", jag_3d%get_num_2d(1), 4)
  call add_test("2", jag_3d%get_num_2d(2), 2)
  call add_test("3", jag_3d%get_num_2d(3), 3)
  call add_test("4", jag_3d%get_num_2d(4), 3)
  call add_test("5", jag_3d%get_num_2d(5), 0)
  call end_tests()

  call start_tests("get_num_3d")
  call add_test("(1, 1)", jag_3d%get_num_3d(1, 1), 3)
  call add_test("(1, 2)", jag_3d%get_num_3d(1, 2), 3)
  call add_test("(1, 3)", jag_3d%get_num_3d(1, 3), 1)
  call add_test("(1, 4)", jag_3d%get_num_3d(1, 4), 2)
  call add_test("(2, 1)", jag_3d%get_num_3d(2, 1), 2)
  call add_test("(2, 2)", jag_3d%get_num_3d(2, 2), 2)
  call add_test("(3, 1)", jag_3d%get_num_3d(3, 1), 3)
  call add_test("(3, 2)", jag_3d%get_num_3d(3, 2), 3)
  call add_test("(3, 3)", jag_3d%get_num_3d(3, 3), 2)
  call add_test("(4, 1)", jag_3d%get_num_3d(4, 1), 3)
  call add_test("(4, 2)", jag_3d%get_num_3d(4, 2), 0)
  call add_test("(4, 3)", jag_3d%get_num_3d(4, 3), 4)
  call end_tests()

  !array[1][1] = new string[] { "111", "112", "113" };
  !array[1][2] = new string[] { "121", "122", "123" };
  !array[1][3] = new string[] { "131" };
  !array[1][4] = new string[] { "141", "142" };

  !array[2][1] = new string[] { "211", "212" };
  !array[2][2] = new string[] { "221", "222" };

  !array[3][1] = new string[] { "311", "312", "313" };
  !array[3][2] = new string[] { "321", "322", "323" };
  !array[3][3] = new string[] { "331", "332" };

  !array[4][1] = new string[] {  "411", "412", "413" };
  !array[4][2] = new string[] { };
  !array[4][3] = new string[] {  "431", "432", "433", "434" };

end program