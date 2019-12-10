module ordering_m
  use contiguous_sets_m
  use jagged_array_m
  use sym_to_asym_m
  implicit none
  
  public :: Metis_ordering

contains

  subroutine Metis_ordering(sym_ccs, perm, iperm)
    type(jagged_array_c), pointer :: sym_ccs
    integer, pointer, contiguous, intent(out) :: perm(:), iperm(:)
    integer, pointer, contiguous :: options(:), ccs_col(:), ccs_row(:)
    type(contiguous_sets_c), pointer :: ccs_set
    type(jagged_array_c), pointer :: asym_ccs
    integer, pointer :: vwgt(:) => null()
    integer :: n

    n = sym_ccs%get_num_arrays()
    ! REVIEW:初期は必要かどうか確認する必要あり
    allocate(perm(n), source=0)
    allocate(iperm(n), source=0)

    call sym_to_asym(sym_ccs, asym_ccs)

    allocate(options(0:40))
    call METIS_SetDefaultOptions(options)
    options(17) = 1 !fortranなので1始まり
    ccs_set => asym_ccs%get_set()
    ccs_col => ccs_set%get_raw()
    ccs_row => asym_ccs%get_raw_val()

    call METIS_NodeND(n, ccs_col, ccs_row, vwgt, options, perm, iperm)

  end subroutine
end module