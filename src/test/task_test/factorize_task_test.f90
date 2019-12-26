program factorize_task_test
  use starpu_wrapper_m
  use factorize_tasks_m, task_init => init, task_finalize => finalize
  use test_util
  use iso_c_binding
  implicit none
  call starpu_init
  call task_init

  call factorize_test
  call solve_test
  call sym_update_test
  call update_test

  call task_finalize
  call starpu_finalize

contains
  subroutine factorize_test
    !  1   2   3      1
    !  2   8  12  =>  2  2
    !  3  12  27      3  3  3
    double precision, pointer, contiguous :: a(:)
    type(c_ptr) :: a_dh
    
    allocate(a, source=[double precision::1,0,0,2,8,0,3,12,27])
    call register_vector_data(a_dh, a)
    call factorize_task%insert_task((/3/), (/a_dh/))
    call task_wait_for_all
    call assert_equal("factorize", a, [double precision::1,0,0,2,2,0,3,3,3])
    
  end subroutine

  subroutine solve_test
    !  [1      ] [1  4]     [ 1   4]
    !  [2  2   ] [2  5]  =  [ 6  18]
    !  [3  3  3] [3  6]     [18  45]
    double precision, pointer, contiguous :: a(:), b(:)
    type(c_ptr) :: a_dh, b_dh

    allocate(a, source=[double precision::1,0,0,2,2,0,3,3,3])
    allocate(b, source=[double precision::1,6,18,4,18,45])

    call register_vector_data(a_dh, a)
    call register_vector_data(b_dh, b)
    call solve_task%insert_task((/3, 2/), (/a_dh, b_dh/))
    call task_wait_for_all
    call assert_equal("solve", b, [double precision::1,2,3,4,5,6])
    
  end subroutine

  subroutine sym_update_test
    !  [1  2  3] [1  4]     [14   32]
    !  [4  5  6] [2  5]  =  [32   77]
    !            [3  6]
    double precision, pointer, contiguous :: a(:), b(:)
    type(c_ptr) :: a_dh, b_dh

    allocate(a, source=[double precision::1,2,3,4,5,6])
    allocate(b(4), source=1d0)

    call register_vector_data(a_dh, a)
    call register_vector_data(b_dh, b)
    call sym_update_task%insert_task((/2,3/),(/a_dh, b_dh/))
    call task_wait_for_all
    call assert_equal("sym_update", b, [-13d0, 1d0, -31d0, -76d0])
    
  end subroutine

  subroutine update_test
    !         [ 1  4  7  10]
    ! [1 2 3] [ 2  5  8  11] = [14 32  50  68]
    ! [4 5 6] [ 3  6  9  12]   [32 76 122 166]
    ! <=>
    ! [ 1  2  3] [1 4]   [14  32]
    ! [ 4  5  6] [2 5] = [32  77]
    ! [ 7  8  9] [3 6]   [50 122]
    ! [10 11 12]         [68 167]
    double precision, pointer, contiguous :: lower(:), upper(:), update(:)
    type(c_ptr) :: lower_dh, upper_dh, update_dh
    integer :: i

    allocate(lower, source=[(dble(i),i=1,6)])
    allocate(upper, source=[(dble(i),i=1,12)])
    allocate(update(8), source=1d0)
    call register_vector_data(lower_dh, lower)
    call register_vector_data(upper_dh, upper)
    call register_vector_data(update_dh, update)
    call update_task%insert_task((/3,2,4/),(/lower_dh, upper_dh, update_dh/))
    call task_wait_for_all
    call assert_equal("update", update, [double precision::-13,-31,-49,-67,-31,-76,-121,-166])

  end subroutine
  
end program