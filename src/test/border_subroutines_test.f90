program border_subroutines_test
  ! ssize=3, nb=5
  ! [ 1                                  ]     ! [ 1                                         ]
  ! [ 2  8                               ]     ! [ 2  2                                      ]
  ! [ 3 12 27                            ]     ! [ 3  3  3                                   ]
  ! [ 1  4  9  0                         ] =>  ! [ 1  1  1  -3                               ]
  ! [ 2  8 18  0  0                      ]     ! [ 2  2  2  -6 -12                           ]
  ! [ 3 12 27  0  0  0                   ]     ! [ 3  3  3  -9 -18 -27                       ]
  ! [ 1  4  9  0  0  0  0                ]     ! [ 1  1  1  -3  -6  -9 -3                    ]
  ! [ 2  8 18  0  0  0  0  0             ]     ! [ 2  2  2  -6 -12 -18 -6 -12                ]
  ! [ 3 12 27  0  0  0  0  0  0          ]     ! [ 3  3  3  -9 -18 -27 -9 -18 -27            ]
  ! [ 1  4  9  0  0  0  0  0  0  0       ]     ! [ 1  1  1  -3  -6  -9 -3  -6  -9 -3         ]
  ! [ 2  8 18  0  0  0  0  0  0  0  0    ]     ! [ 2  2  2  -6 -12 -18 -6 -12 -18 -6 -12     ]
  ! [ 3 12 27  0  0  0  0  0  0  0  0  0 ]     ! [ 3  3  3  -9 -18 -27 -9 -18 -27 -9 -18 -27 ]
  
  use factors_m
  use contiguous_sets_m
  use jagged_array_m
  use border_subroutines_m
  use test_util
  implicit none
  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_sets
  type(jagged_array_c), pointer :: ccs
  integer, pointer, contiguous :: ccs_val(:)
  double precision, pointer, contiguous :: a11_s(:), a11_w(:), a21_s(:), a21_w(:), a31_s(:), a31_w(:), a22(:), a32(:), a33(:)
  node_sets => create_contiguous_sets((/3, 9/))
  
  allocate(ccs_val(9))
  ccs_val = (/4, 5, 6, 7, 8, 9, 10, 11, 12/)
  ccs => create_jagged_array((/9, 0/), ccs_val)

  factors => create_factors(node_sets, ccs, 5)

  a11_s => factors%get_supernode_ptr(1, 1, 1)
  a21_s => factors%get_supernode_ptr(1, 2, 1)
  a31_s => factors%get_supernode_ptr(1, 3, 1)
  a11_w => factors%get_work_ptr(1, 1, 1)
  a21_w => factors%get_work_ptr(1, 2, 1)
  a31_w => factors%get_work_ptr(1, 3, 1)
  a22 => factors%get_matrix_ptr(1, 2, 2)
  a32 => factors%get_matrix_ptr(1, 3, 2)
  a33 => factors%get_matrix_ptr(1, 3, 3)

  a11_s(1)=1;  a11_s(4)=2;  a11_s(5)=8
  a11_s(7)=3;  a11_s(8)=12; a11_s(9)=27
  a11_s(10)=1; a11_s(11)=4; a11_s(12)=9;
  a11_s(13)=2; a11_s(14)=8; a11_s(15)=18;

  a21_s(1)=3;  a21_s(2)=12;  a21_s(3)=27;
  a21_s(4)=1;  a21_s(5)=4;   a21_s(6)=9;
  a21_s(7)=2;  a21_s(8)=8;   a21_s(9)=18;
  a21_s(10)=3; a21_s(11)=12; a21_s(12)=27;
  a21_s(13)=1; a21_s(14)=4;  a21_s(15)=9;
  
  a31_s(1)=2;  a31_s(2)=8;   a31_s(3)=18;
  a31_s(4)=3;  a31_s(5)=12;  a31_s(6)=27;
  
  a11_w=0; a21_w=0; a31_w=0
  a22=0; a32=0; a33=0

  call border_factorize(factors, 1, 1)
  call assert_equal_partial_array("factorize:a11_s", a11_s, &
    &(/1, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15/), 12, &
    &(/1d0, 2d0, 2d0, 3d0, 3d0, 3d0, 1d0, 1d0, 1d0, 2d0, 2d0, 2d0/))
  call assert_equal_partial_array("factorize:a11_w", a11_w, (/1, 3, 4/), 3, (/-3d0, -6d0, -12d0/))

  call border_solve(factors, 1, 2, 1)
  call assert_equal("solve:a21_s", a21_s, (/3d0, 3d0, 3d0, 1d0, 1d0, 1d0, 2d0, 2d0, 2d0, 3d0, 3d0, 3d0, 1d0, 1d0, 1d0/))
  call assert_equal("solve:a21_w", a21_w, (/-9d0, -18d0, -3d0, -6d0, -6d0, -12d0, -9d0, -18d0, -3d0, -6d0/))
  call border_solve(factors, 1, 3, 1)
  call assert_equal("solve:a31_s", a31_s, (/2d0, 2d0, 2d0, 3d0, 3d0, 3d0/))
  call assert_equal("solve:a31_w", a31_w, (/-6d0, -12d0, -9d0, -18d0/))
  call border_sym_update(factors, 1, 2, 1)
  call assert_equal_partial_array("sym_update:a22", a22, &
    &(/1, 6, 7, 11, 12, 13, 16, 17, 18, 19, 21, 22, 23, 24, 25/), 10, &
    &(/-27d0, -9d0, -3d0, -18d0, -6d0, -12d0, -27d0, -9d0, -18d0, -27d0, -9d0, -3d0, -6d0, -9d0, -3d0/))
  call border_sym_update(factors, 1, 3, 1)
  call assert_equal_partial_array("sym_update:a33", a33, (/1, 3, 4/), 3, (/-12d0, -18d0, -27d0/))
  call border_update(factors, 1, 2, 3, 1)
  call assert_equal("update:a32", a32, (/-18d0, -6d0, -12d0, -18d0, -6d0, -27d0, -9d0, -18d0, -27d0, -9d0/))

end program border_subroutines_test