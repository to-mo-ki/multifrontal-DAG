program main
  use hppllt
  use test_util
  use sparse_matrix_maker_m
  implicit none
  integer, pointer, contiguous :: coo_col(:), coo_row(:), ccs_col(:), ccs_row(:)
  double precision, pointer, contiguous :: coo_val(:), ccs_val(:), b(:)
  double precision :: rh, error
  integer :: n, nonzero, max_zero, nb
  character(len=128) :: file_name

  call test("bcsstk01.mtx", 10, 10)
  call test("poisson_9_9.mtx", 10, 10)

contains
  subroutine test(matrix_name, arg_max_zero, arg_nb)
    use to_str_m
    character(*) :: matrix_name
    integer :: arg_max_zero, arg_nb
    integer, allocatable :: options(:)
    file_name="./test_matrix/"//matrix_name
    nb = arg_nb
    max_zero = arg_max_zero

    call read_data
    call coo_to_ccs
    rh = 10d0
    allocate(b(n), source=rh)
    allocate(options, source=[nb, max_zero, 1])
    call hppllt_init(options)
    call hppllt_analyze(ccs_col, ccs_row, n)
    call hppllt_factorize(ccs_val)
    call hppllt_solve(b)
    call hppllt_finalize
    
    error = calc_error()
    
    call assert_equal(matrix_name//", max_zero="//to_str(max_zero)//", nb="//to_str(nb), error, 0d0, in_ignore_digits=1D-8)

  end subroutine

  subroutine read_data
    integer :: i,ios, nh
    character(len=128) :: arg

    open(1, file=file_name, iostat=ios, status='old')
    if(ios /= 0)then
      print *, "cannot open:", file_name
      stop
    endif
    !ヘッダーを読み飛ばす
    nh = 0
    do
      read(1,'(A)')arg
      if(arg(1:1) == "%")then
        nh=nh+1
      else
        rewind(1)
        exit
      endif
    enddo
    do i=1,nh
      read(1,*,iostat=ios)
    enddo

    read(1,*,iostat=ios) n,n,nonzero
    allocate(coo_row(nonzero), coo_col(nonzero), coo_val(nonzero))
    do i=1,nonzero
      read(1,*,iostat=ios) coo_row(i),coo_col(i), coo_val(i)
    enddo
    close(1)

  end subroutine read_data

  subroutine coo_to_ccs()
    integer, allocatable :: ncol(:), row_num(:), tmp_row(:), tmp_col(:)
    double precision, allocatable :: tmp_val(:)
    integer :: i
    
    allocate(ncol(n), row_num(n), tmp_row(nonzero), tmp_col(nonzero), tmp_val(nonzero))

    allocate(ccs_col(n+1),ccs_row(nonzero), ccs_val(nonzero))

    !1:key counting
    ncol = 0
    row_num = 0
    do i=1,nonzero
      if(coo_col(i) == 0)then
        print *, i
        cycle
      endif
      ncol(coo_col(i)) = ncol(coo_col(i)) + 1
      row_num(coo_row(i)) = row_num(coo_row(i)) + 1
    enddo
    !2:calc location(next)
    row_num(1) = row_num(1) + 1
    do i=2,n
      row_num(i) = row_num(i) + row_num(i-1)
    enddo
    ncol(1) = ncol(1) + 1
    do i=2,n
      ncol(i) = ncol(i) + ncol(i-1)
    enddo
    !3:set col
    ccs_col(1) = 1
    do i=1,n
      ccs_col(i+1) = ncol(i)
    enddo

    !4:sort by rows
    do i=1,nonzero
      row_num(coo_row(i)) = row_num(coo_row(i))-1
      tmp_row(row_num(coo_row(i))) = coo_row(i)
      tmp_col(row_num(coo_row(i))) = coo_col(i)
      tmp_val(row_num(coo_row(i))) = coo_val(i)
    enddo

    !4:set row
    do i=nonzero,1,-1
      ncol(tmp_col(i)) = ncol(tmp_col(i))-1
      ccs_row(ncol(tmp_col(i))) = tmp_row(i)
      ccs_val(ncol(tmp_col(i))) = tmp_val(i)
    enddo

  end subroutine

  double precision function calc_error result(error)
    double precision :: sum_b
    double precision, allocatable :: tmp(:)
    integer :: i, j
  
    allocate(tmp(n))
  
    tmp = 0.0d0
    error = 0.0d0
  
    do j=1,n
      do i=ccs_col(j)+1, ccs_col(j+1)-1
        tmp(ccs_row(i)) = tmp(ccs_row(i)) + ccs_val(i)*b(j)
      enddo
    enddo
  
    do j=1,n
      do i=ccs_col(j), ccs_col(j+1)-1
        tmp(j) = tmp(j) + ccs_val(i)*b(ccs_row(i))
      enddo
    enddo
    
    do i=1,n
      error = error + (rh - tmp(i))**2
    enddo
    
    sum_b = 0.0d0
    do i=1,n
      sum_b = sum_b + b(i)**2
    enddo
  
    error = sqrt(error)/sqrt(sum_b)
  
  end function

end program
