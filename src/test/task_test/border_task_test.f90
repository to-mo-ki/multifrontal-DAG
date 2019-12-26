program border_task_test
  use starpu_wrapper_m
  use border_tasks_m, task_init => init, task_finalize => finalize
  use test_util
  use iso_c_binding
  implicit none
  
  call starpu_init
  call task_init
  call factorize_test
  call solve_test1
  call solve_test2
  call task_finalize
  call starpu_finalize
  
contains
subroutine factorize_test
    ! border(3:2)
    !  [1                 ]      [1               ]
    !  [2   8             ]      [2  2            ]
    !  [3  12  27         ]  =>  [3  3  3         ]
    !  [4  16  36  64     ]      [4  4  4  16     ]
    !  [5  20  45  80  125]      [5  5  5  20  50 ]
    double precision, pointer, contiguous :: a(:), b(:)
    type(c_ptr) :: a_dh, b_dh


    allocate(a, source=[double precision::1,0,0,2,8,0,3,12,27,4,16,36,5,20,45])
    allocate(b, source=[double precision::64,0,80,125])

    a_dh = register_vector_data(a)
    b_dh = register_vector_data(b)
    
    call border_factorize_task%insert_task((/3,2/),(/a_dh, b_dh/))
    call task_wait_for_all
    
    call assert_equal("factorizet:a", a, [double precision::1,0,0,2,2,0,3,3,3,4,4,4,5,5,5])
    call assert_equal("factorize:b", b, [double precision::16,0,20,50])

  end subroutine


  subroutine solve_test1
    !     [1            ]  LOWER=[1   6  18  0  0] 
    !     [2  2         ]        [4  18  45  0  0] 
    !DIAG=[3  3  3      ]  
    !     [4  4  4  *   ]  ANSWER= [1  2  3  -24  -30] 
    !     [5  5  5  *  *]          [4  5  6  -60  -75] 

    double precision, pointer, contiguous :: diag_a(:), lower_a(:), lower_b(:)
    type(c_ptr) :: diag_a_dh, lower_a_dh, lower_b_dh
    
    allocate(diag_a, source=[double precision::1,0,0,2,2,0,3,3,3,4,4,4,5,5,5])
    allocate(lower_a, source=[double precision::1,6,18,4,18,45])
    allocate(lower_b(4), source=0d0)

    diag_a_dh = register_vector_data(diag_a)
    lower_a_dh = register_vector_data(lower_a)
    lower_b_dh = register_vector_data(lower_b)
    
    call border_solve_task%insert_task((/3,2,2/),(/diag_a_dh, lower_a_dh, lower_b_dh/))
    call task_wait_for_all

    call assert_equal("solve:1a", lower_a, [double precision::1,2,3,4,5,6])
    call assert_equal("solve:1b", lower_b, [double precision::-24,-30,-60,-75])
    
  end subroutine

  subroutine solve_test2
    !     [1            ]  LOWER=[1   6  0  0  0] 
    !     [2  2         ]        [4  18  0  0  0] 
    !DIAG=[3  3  *      ]  
    !     [4  4  *  *   ]  ANSWER= [1  2 -21 -28   -35] 
    !     [5  5  *  *  *]          [4  5 -66 -88  -110] 

    double precision, pointer, contiguous :: diag_a(:), lower_a(:), lower_b(:)
    type(c_ptr) :: diag_a_dh, lower_a_dh, lower_b_dh
    
    allocate(diag_a, source=[double precision::1,0,2,2d0,3,3,4,4,5,5])
    allocate(lower_a, source=[double precision::1,6,4,18])
    allocate(lower_b(6), source=0d0)

    diag_a_dh = register_vector_data(diag_a)
    lower_a_dh = register_vector_data(lower_a)
    lower_b_dh = register_vector_data(lower_b)
    
    call border_solve_task%insert_task((/2,3,2/),(/diag_a_dh, lower_a_dh, lower_b_dh/))
    call task_wait_for_all

    call assert_equal("solve1:a", lower_a, [double precision::1,2,4,5d0 ])
    call assert_equal("solve1:b", lower_b, [double precision::-9,-12,-15,-27,-36,-45])
    
  end subroutine
end program