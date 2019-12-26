program starpu_factorize_subroutines_test
  ! [ 1                      ]     ! [ 1                          ]
  ! [ 2  8                   ]     ! [ 2  2                       ]
  ! [ 3 12 27                ]     ! [ 3  3  3                    ]
  ! [ 1  4  9  0             ] =>  ! [ 1  1  1  -3                 ]
  ! [ 2  8 18  0  0          ]     ! [ 2  2  2  -6 -12             ]
  ! [ 3 12 27  0  0  0       ]     ! [ 3  3  3  -9 -18 -27         ]
  ! [ 4 16 36  0  0  0  0    ]     ! [ 4  4  4 -12 -24 -36 -48     ]
  ! [ 5 20 45  0  0  0  0  0 ]     ! [ 5  5  5 -15 -30 -45 -60 -75 ]
  use factors_m
  use node_data_m
  use starpu_factorize_subroutines_m
  use factorize_tasks_m, factorize_init => init, factorize_finalize => finalize
  use starpu_factors_m
  use register_factors_m
  use test_util
  use starpu_wrapper_m
  implicit none
  type(factors_c), pointer :: factors
  type(starpu_factors_c), pointer :: starpu_factors

  type(node_data_c), pointer :: node_data
  double precision, pointer, contiguous :: a11(:), a21(:), a31(:), a22(:), a32(:), a33(:)

  call starpu_init
  call factorize_init
  
  node_data => create_node_data([3,5],[5,0],3)
  factors => create_factors(node_data)

  a11 => factors%get_matrix(1, 1, 1)
  a21 => factors%get_matrix(1, 2, 1)
  a31 => factors%get_matrix(1, 3, 1)
  a22 => factors%get_matrix(1, 2, 2)
  a32 => factors%get_matrix(1, 3, 2)
  a33 => factors%get_matrix(1, 3, 3)

  a11(1)=1; a11(4)=2;  a11(5)=8
  a11(7)=3; a11(8)=12; a11(9)=27
  a21(1)=1; a21(2)=4;  a21(3)=9;
  a21(4)=2; a21(5)=8;  a21(6)=18;
  a21(7)=3; a21(8)=12; a21(9)=27;
  a31(1)=4; a31(2)=16; a31(3)=36;
  a31(4)=5; a31(5)=20; a31(6)=45;

  a22=0; a32=0; a33=0

  
  starpu_factors => create_starpu_factors(node_data)
  call register_factors(node_data, starpu_factors, factors)
  
  call factorize(node_data, starpu_factors, 1, 1)
  call task_wait_for_all
  call assert_equal_partial_array("factorize:a11", a11, [1, 4, 5, 7, 8, 9], 6, [1d0, 2d0, 2d0, 3d0, 3d0, 3d0])
  call solve(node_data, starpu_factors, 1, 2, 1)
  call task_wait_for_all
  call assert_equal("solve:a21", a21, [1d0, 1d0, 1d0, 2d0, 2d0, 2d0, 3d0, 3d0, 3d0])
  call solve(node_data, starpu_factors, 1, 3, 1)
  call task_wait_for_all
  call assert_equal("solve:a31", a31, [4d0, 4d0, 4d0, 5d0, 5d0, 5d0])
  call sym_update(node_data, starpu_factors, 1, 2, 1)
  call task_wait_for_all
  call assert_equal_partial_array("sym_update:a22", a22, [1, 4, 5, 7, 8, 9], 6, [-3d0, -6d0, -12d0, -9d0, -18d0, -27d0])
  call sym_update(node_data, starpu_factors, 1, 3, 1)
  call task_wait_for_all
  call assert_equal_partial_array("sym_update:a33", a33, [1, 3, 4], 3, [-48d0, -60d0, -75d0])
  call update(node_data, starpu_factors, 1, 2, 3, 1)
  call task_wait_for_all
  call assert_equal("update:a32", a32, [-12d0, -24d0, -36d0, -15d0, -30d0, -45d0])

  call factorize_finalize
  call starpu_finalize
  
end program