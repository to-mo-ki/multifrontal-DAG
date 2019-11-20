program extend_add_test
  use extend_add_kernel_m
  use test_util
  implicit none
  double precision, allocatable :: from(:), to(:)
  integer, allocatable :: local(:), col_local(:), row_local(:)

  call test_tri

contains
  subroutine test_tri
    allocate(from(4*4), source=[double precision :: 22,0,0,0,32,33,0,0,72,73,77,0,82,83,87,88])
    allocate(to(9*9), source=100d0)
    allocate(local(4), source=[2,3,7,8])
    call extend_add_tri(from, to, local, 4, 9)

    call start_tests("tri")
    call add_test("(2, 2)", to(2+(2-1)*9), 122d0)
    call add_test("(3, 2)", to(2+(3-1)*9), 132d0)
    call add_test("(7, 2)", to(2+(7-1)*9), 172d0)
    call add_test("(8, 2)", to(2+(8-1)*9), 182d0)
    call add_test("(3, 3)", to(3+(3-1)*9), 133d0)
    call add_test("(7, 3)", to(3+(7-1)*9), 173d0)
    call add_test("(8, 3)", to(3+(8-1)*9), 183d0)
    call add_test("(7, 7)", to(7+(7-1)*9), 177d0)
    call add_test("(8, 7)", to(7+(8-1)*9), 186d0)
    call end_tests()

  end subroutine
  
end program extend_add_test