program main
  use omp_lib
  use hppllt
  use fstarpu_mod
  implicit none
  integer, pointer, contiguous :: coo_col(:), coo_row(:), ccs_col(:), ccs_row(:)
  double precision, pointer, contiguous :: coo_val(:), ccs_val(:), b(:)
  double precision :: rh, error
  integer :: n, nonzero, max_zero, nb, i
  integer, allocatable :: options(:)
  character(len=128) :: file_name, arg, data_name
  
  if(iargc() /= 4)then
    print *,"command line argument error"
    stop
  endif

  call getarg(1, file_name)

  call getarg(2,arg)
  read(arg,*) max_zero

  call getarg(3,arg)
  read(arg,*) nb

  call getarg(4,data_name)

  call read_data
  call coo_to_ccs

  rh = ccs_val(1)
  allocate(b(n), source=rh)
  allocate(options, source=[nb, max_zero, 1, 1])
  print *,"I"
  call hppllt_init(options)
  print *,"A"
  call hppllt_analyze(ccs_col, ccs_row, n)
  print *,"F"
  call hppllt_factorize(ccs_val)
  print *,"S"
  call hppllt_solve(b)
  
  call hppllt_finalize
  
  error = calc_error()

  open(7, file=data_name, status='replace')
  write(7, "(i8)", advance="no") max_zero
  write(7, "(i8)", advance="no") nb
  write(7, "(i8)", advance="no") omp_get_max_threads()
  write(7, "(i12)", advance="no") hppllt_get_nonzero()
  write(7, "(i20)", advance="no") hppllt_get_cost()
  do i=1, 3
    write(7, "(i20)", advance="no") hppllt_get_array_size(i)
  enddo
  do i=1, 7
    write(7, "(f10.3)", advance="no") hppllt_get_time_info(i)
  enddo
  write(7, "(e10.3)", advance="no") error
  close(7)

contains
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
