program factorize_subroutines_test
  ! [ 1                      ]     ! [ 1                          ]
  ! [ 2  8                   ]     ! [ 2  2                       ]
  ! [ 3 12 27                ]     ! [ 3  3  3                    ]
  ! [ 1  4  9  0             ] =>  ! [ 1  1  1  -3                 ]
  ! [ 2  8 18  0  0          ]     ! [ 2  2  2  -6 -12             ]
  ! [ 3 12 27  0  0  0       ]     ! [ 3  3  3  -9 -18 -27         ]
  ! [ 4 16 36  0  0  0  0    ]     ! [ 4  4  4 -12 -24 -36 -48     ]
  ! [ 5 20 45  0  0  0  0  0 ]     ! [ 5  5  5 -15 -30 -45 -60 -75 ]
  use factors_m
  use contiguous_sets_m
  use jagged_array_m
  use node_data_m
  use factorize_subroutines_m
  use test_util
  implicit none
  type(factors_c), pointer :: factors
  type(contiguous_sets_c), pointer :: node_sets
  type(node_data_c), pointer :: node_data
  type(jagged_array_c), pointer :: ccs
  integer, pointer, contiguous :: ccs_val(:)
  double precision, pointer, contiguous :: a11(:), a21(:), a31(:), a22(:), a32(:), a33(:)

  node_sets => create_contiguous_sets([3, 5])
  
  allocate(ccs_val(5))
  ccs_val = [4, 5, 6, 7, 8]
  ccs => create_jagged_array([5, 0], ccs_val)
  node_data => create_node_data([3,5],[5,0],3)
  factors => create_factors(node_data, ccs, 3)

  a11 => factors%get_matrix_ptr(1, 1, 1)
  a21 => factors%get_matrix_ptr(1, 2, 1)
  a31 => factors%get_matrix_ptr(1, 3, 1)
  a22 => factors%get_matrix_ptr(1, 2, 2)
  a32 => factors%get_matrix_ptr(1, 3, 2)
  a33 => factors%get_matrix_ptr(1, 3, 3)

  a11(1)=1; a11(4)=2;  a11(5)=8
  a11(7)=3; a11(8)=12; a11(9)=27
  a21(1)=1; a21(2)=4;  a21(3)=9;
  a21(4)=2; a21(5)=8;  a21(6)=18;
  a21(7)=3; a21(8)=12; a21(9)=27;
  a31(1)=4; a31(2)=16; a31(3)=36;
  a31(4)=5; a31(5)=20; a31(6)=45;

  a22=0; a32=0; a33=0

  call factorize(factors, 1, 1)
  call assert_equal_partial_array("factorize:a11", a11, [1, 4, 5, 7, 8, 9], 6, [1d0, 2d0, 2d0, 3d0, 3d0, 3d0])
  call solve(factors, 1, 2, 1)
  call assert_equal("solve:a21", a21, [1d0, 1d0, 1d0, 2d0, 2d0, 2d0, 3d0, 3d0, 3d0])
  call solve(factors, 1, 3, 1)
  call assert_equal("solve:a31", a31, [4d0, 4d0, 4d0, 5d0, 5d0, 5d0])
  call sym_update(factors, 1, 2, 1)
  call assert_equal_partial_array("sym_update:a22", a22, [1, 4, 5, 7, 8, 9], 6, [-3d0, -6d0, -12d0, -9d0, -18d0, -27d0])
  call sym_update(factors, 1, 3, 1)
  call assert_equal_partial_array("sym_update:a33", a33, [1, 3, 4], 3, [-48d0, -60d0, -75d0])
  call update(factors, 1, 2, 3, 1)
  call assert_equal("update:a32", a32, [-12d0, -24d0, -36d0, -15d0, -30d0, -45d0])

  
end program