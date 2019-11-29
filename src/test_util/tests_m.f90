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
  end interface

  public :: add_test
  public :: start_tests, end_tests, start_array_tests, end_array_tests

end module