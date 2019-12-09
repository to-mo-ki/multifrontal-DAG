module tests_m
  use scalar_tests_m
  use array_tests_m
  private

  interface add_test
    procedure :: add_test1
    procedure :: add_test2
    procedure :: add_test3
    procedure :: add_test4
    procedure :: add_test5
    procedure :: add_test6
    procedure :: add_test7
  end interface

  public :: add_test, add_test_tri
  public :: start_tests, end_tests, start_array_tests, end_array_tests

end module