program extend_add_test
  use extend_add_kernel_m
  use test_util
  implicit none
  double precision, allocatable :: from(:), to(:)
  integer, allocatable :: local(:), col_local(:), row_local(:)

  call test_tri
  call test_rect1
  call test_rect2

contains
  subroutine test_tri
    allocate(from(4*4))
    from = [22,0,0,0,32,33,0,0,72,73,77,0,82,83,87,88]
    allocate(to(9*9), source=100d0)
    allocate(local(4), source=[2,3,7,8])
    call extend_add_tri(from, to, local, 4, 9, 0)

    call start_tests("tri")
    call add_test("(2, 2)", to(2+(2-1)*9), 122d0)
    call add_test("(3, 2)", to(2+(3-1)*9), 132d0)
    call add_test("(7, 2)", to(2+(7-1)*9), 172d0)
    call add_test("(8, 2)", to(2+(8-1)*9), 182d0)
    call add_test("(3, 3)", to(3+(3-1)*9), 133d0)
    call add_test("(7, 3)", to(3+(7-1)*9), 173d0)
    call add_test("(8, 3)", to(3+(8-1)*9), 183d0)
    call add_test("(7, 7)", to(7+(7-1)*9), 177d0)
    call add_test("(8, 7)", to(7+(8-1)*9), 187d0)
    call end_tests()

    deallocate(from, to, local)

  end subroutine

  subroutine test_rect1
    allocate(from(5*4))
    from = [21,23,26,28,29,31,33,36,38,39,71,73,76,78,79,81,83,86,88,89]
    allocate(to(9*9), source=100d0)
    allocate(col_local(5), source=[1,3,6,8,9])
    allocate(row_local(4), source=[2,3,7,8])
    call extend_add_rect(from, to, col_local, row_local, 5, 9, 0, 0)

    call start_tests("rect1:4*5")
    call add_test("(2, 1)", to(1+(2-1)*9), 121d0)
    call add_test("(3, 1)", to(1+(3-1)*9), 131d0)
    call add_test("(7, 1)", to(1+(7-1)*9), 171d0)
    call add_test("(8, 1)", to(1+(8-1)*9), 181d0)
    call add_test("(2, 3)", to(3+(2-1)*9), 123d0)
    call add_test("(3, 3)", to(3+(3-1)*9), 133d0)
    call add_test("(7, 3)", to(3+(7-1)*9), 173d0)
    call add_test("(8, 3)", to(3+(8-1)*9), 183d0)
    call add_test("(2, 6)", to(6+(2-1)*9), 126d0)
    call add_test("(3, 6)", to(6+(3-1)*9), 136d0)
    call add_test("(7, 6)", to(6+(7-1)*9), 176d0)
    call add_test("(8, 6)", to(6+(8-1)*9), 186d0)
    call add_test("(2, 8)", to(8+(2-1)*9), 128d0)
    call add_test("(3, 8)", to(8+(3-1)*9), 138d0)
    call add_test("(7, 8)", to(8+(7-1)*9), 178d0)
    call add_test("(8, 8)", to(8+(8-1)*9), 188d0)
    call add_test("(2, 9)", to(9+(2-1)*9), 129d0)
    call add_test("(3, 9)", to(9+(3-1)*9), 139d0)
    call add_test("(7, 9)", to(9+(7-1)*9), 179d0)
    call add_test("(8, 9)", to(9+(8-1)*9), 189d0)

    call end_tests()

    deallocate(from, to, col_local, row_local)

  end subroutine

  subroutine test_rect2
    allocate(from(4*5))
    from = [12,13,17,18,32,33,37,38,62,63,67,68,82,83,87,88,92,93,97,98]
    allocate(to(9*9), source=100d0)
    allocate(col_local(4), source=[2,3,7,8])
    allocate(row_local(5), source=[1,3,6,8,9])
    call extend_add_rect(from, to, col_local, row_local, 4, 9, 0, 0)

    call start_tests("rect2:5*4")
    call add_test("(1, 2)", to(2+(1-1)*9), 112d0)
    call add_test("(3, 2)", to(2+(3-1)*9), 132d0)
    call add_test("(6, 2)", to(2+(6-1)*9), 162d0)
    call add_test("(8, 2)", to(2+(8-1)*9), 182d0)
    call add_test("(9, 2)", to(2+(9-1)*9), 192d0)
    call add_test("(1, 3)", to(3+(1-1)*9), 113d0)
    call add_test("(3, 3)", to(3+(3-1)*9), 133d0)
    call add_test("(6, 3)", to(3+(6-1)*9), 163d0)
    call add_test("(8, 3)", to(3+(8-1)*9), 183d0)
    call add_test("(9, 3)", to(3+(9-1)*9), 193d0)
    call add_test("(1, 7)", to(7+(1-1)*9), 117d0)
    call add_test("(3, 7)", to(7+(3-1)*9), 137d0)
    call add_test("(6, 7)", to(7+(6-1)*9), 167d0)
    call add_test("(8, 7)", to(7+(8-1)*9), 187d0)
    call add_test("(9, 7)", to(7+(9-1)*9), 197d0)
    call add_test("(1, 8)", to(8+(1-1)*9), 118d0)
    call add_test("(3, 8)", to(8+(3-1)*9), 138d0)
    call add_test("(6, 8)", to(8+(6-1)*9), 168d0)
    call add_test("(8, 8)", to(8+(8-1)*9), 188d0)
    call add_test("(9, 8)", to(8+(9-1)*9), 198d0)

    call end_tests()

    deallocate(from, to, col_local, row_local)

  end subroutine

  
end program extend_add_test