program border_kernel_test
  use border_kernel_m
  use test_util
  implicit none

  call potrf_test()
  call trsm_test()
contains

  subroutine potrf_test()
    ! border(3:2)
    !  [1                 ]      [1               ]
    !  [2   8             ]      [2  2            ]
    !  [3  12  27         ]  =>  [3  3  3         ]
    !  [4  16  36  64     ]      [4  4  4  16     ]
    !  [5  20  45  80  125]      [5  5  5  20  50 ]
    double precision, pointer, contiguous :: a(:), b(:)
    double precision :: a_check(12), b_check(3)
    integer :: a_pos(12), b_pos(3)

    allocate(a(15), b(4))
    a = (/1d0, 0d0, 0d0, 2d0, 8d0, 0d0, 3d0, 12d0, 27d0, 4d0, 16d0, 36d0, 5d0, 20d0, 45d0/)
    b = (/64d0, 0d0, 80d0, 125d0/)

    call border_potrf(a, b, 3, 2)
    a_pos = (/1, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15/)
    a_check = (/1d0, 2d0, 2d0, 3d0, 3d0, 3d0, 4d0, 4d0, 4d0, 5d0, 5d0, 5d0/)
    call assert_equal_partial_array("dpotrf test:a", a, a_pos, 12, a_check)

    b_pos = (/1, 3, 4/)
    b_check = (/16d0, 20d0, 50d0/)
    call assert_equal_partial_array("dpotrf test:b", b, b_pos, 3, b_check)

  end subroutine


  subroutine trsm_test()
    !     [1            ]  LOWER=[1   6  18  0  0] 
    !     [2  2         ]        [4  18  45  0  0] 
    !DIAG=[3  3  3      ]  
    !     [4  4  4  *   ]  ANSWER= [1  2  3  -24  -60] 
    !     [5  5  5  *  *]          [4  5  6  -30  -75] 

    double precision, pointer, contiguous :: diag_a(:), lower_a(:), lower_b(:)
    
    
    allocate(diag_a(15), lower_a(6), lower_b(4))
    diag_a = (/1d0, 0d0, 0d0, 2d0, 2d0, 0d0, 3d0, 3d0, 3d0, 4d0, 4d0, 4d0, 5d0, 5d0, 5d0/)
    lower_a = (/1d0, 6d0, 18d0, 4d0, 18d0, 45d0/)
    lower_b = 0d0
    call border_trsm(diag_a, lower_a, lower_b, 3, 2, 2)

    call assert_equal("a", lower_a, (/1d0, 2d0, 3d0, 4d0, 5d0, 6d0/))
    call assert_equal("b", lower_b, (/-24d0, -60d0, -30d0, -75d0/))
    
  end subroutine


  
end program border_kernel_test