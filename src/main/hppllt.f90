module hppllt
  use hppllt_data_m
  implicit none
  private

  public :: hppllt_init, hppllt_analyze, hppllt_factorize, hppllt_solve, hppllt_finalize
  
contains

  subroutine hppllt_init(input_options)
    use options_m, only: NUM_OPTIONS, options
    integer :: input_options(:)
    if(size(input_options) /= NUM_OPTIONS)then
      print *, "incorrect size of options"
      return
    endif
    allocate(options, source=input_options)

  end subroutine

  subroutine hppllt_analyze(ccs_col, ccs_row, n)
    use options_m, only: OPTION_NB, OPTION_MAX_ZERO, OPTION_USE_STARPU, OPTION_USE_METIS, options
    use analyze_phase_m
    use reordering_m
    use perm_m
    use create_local_index_m
    use create_supernodal_index_m
    use ordering_m
    use starpu_wrapper_m, only: starpu_init
    use tasks_m, only: task_init => init
    use register_block_local_index_m
    use register_factors_m
    integer, contiguous :: ccs_col(:), ccs_row(:)
    integer, intent(in) :: n
    type(contiguous_sets_c), pointer :: origin_set
    type(jagged_array_c), pointer :: l_structure
    type(jagged_array_c), pointer :: reordered_ccs
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    integer, pointer, contiguous :: reordering_perm(:), reordering_iperm(:), analyze_perm(:), analyze_iperm(:)
    integer, pointer, contiguous :: reordering_ccs_perm(:), analyze_ccs_perm(:)
    type(jagged_array_c), pointer :: local_index
    integer :: i, nb, max_zero
    type(contiguous_sets_c), pointer :: set
    
    nb = options(OPTION_NB)
    max_zero = options(OPTION_MAX_ZERO)

    origin_set => create_raw_contiguous_sets(ccs_col, n)
    origin_structure => create_jagged_array(origin_set, ccs_row)
    
    if(options(OPTION_USE_METIS) == 1)then
      call Metis_ordering(origin_structure, reordering_perm, reordering_iperm)
      reordered_ccs => reordering_ccs(origin_structure, reordering_perm, reordering_iperm, reordering_ccs_perm)
    else
      reordered_ccs => origin_structure
    endif

    call analyze_phase(reordered_ccs, max_zero, l_structure, node_sets, analyze_perm, parent, tree_child)

    if(options(OPTION_USE_METIS) == 1)then
      call perm_product(reordering_perm, analyze_perm, perm)
    else
      perm => analyze_perm
    endif
    
    call set_iperm(analyze_perm, analyze_iperm)
    a_structure => repostordering_ccs(reordered_ccs, analyze_perm, analyze_iperm, analyze_ccs_perm)

    if(options(OPTION_USE_METIS) == 1)then
      call perm_product(reordering_ccs_perm, analyze_ccs_perm, ccs_perm)
    else
      ccs_perm => analyze_ccs_perm
    endif

    ! TODO:node_sets%get_lengthsを作成する？
    allocate(supernode_size(node_sets%get_num_sets()))
    do i=1, node_sets%get_num_sets()
      supernode_size(i) = node_sets%get_length(i)
    enddo
    allocate(work_size(node_sets%get_num_sets()))

    work_size = l_structure%get_array_lengths()
    node_data => create_node_data(supernode_size, work_size, nb)
    factors => create_factors(node_data)
    rh => create_right_hand(node_data, nb)
    supernodal_index => create_supernodal_index(node_sets, a_structure, l_structure)
    local_index => create_local_index(l_structure, node_sets, tree_child)
    block_local_index_info => create_block_local_index_info(node_data, local_index)
    block_local_index => block_local_index_info%create_block_local_index()

    if(options(OPTION_USE_STARPU) == 1)then
      starpu_factors => create_starpu_factors(node_data)
      set => block_local_index_info%node_ptr
      starpu_block_local_index => create_jagged_array_cptr(set)
      call starpu_init
      call task_init
      call register_factors(node_data, starpu_factors, factors)
      call register_block_local_index(starpu_block_local_index, block_local_index)
    endif

  end subroutine

  subroutine hppllt_factorize(a)
    use coefficient_setter_m
    use seq_factorize_m
    use starpu_factorize_m
    use perm_m
    use reordering_m
    use zero_setter_m
    use options_m, only: options, OPTION_USE_STARPU
    double precision, pointer, contiguous :: a(:)
    double precision, pointer, contiguous :: ccs_val(:)

    allocate(ccs_val(size(ccs_perm)))
    call permutate(ccs_perm, a, ccs_val)
    ccs => create_ccs(supernodal_index, ccs_val)
    call set_zero(node_data, factors)
    call set_coefficient(node_data, ccs, node_sets, factors)
    
    if(options(OPTION_USE_STARPU))then
      call starpu_factorize(node_data, starpu_factors, starpu_block_local_index, block_local_index_info, parent)
    else
      call seq_factorize(node_data, factors, block_local_index, block_local_index_info, parent)
    endif

  end subroutine

  subroutine hppllt_solve(b)
    use seq_forward_m
    use seq_backward_m
    use right_hand_m
    use perm_m
    double precision, contiguous :: b(:)
    double precision, pointer, contiguous :: tmp_b(:)

    allocate(tmp_b(size(perm)))
    call permutate(perm, b, tmp_b)
    call rh%set_val(tmp_b)
    call seq_forward(node_data, factors, rh, block_local_index, block_local_index_info, parent)
    call seq_backward(node_data, factors, rh, block_local_index, block_local_index_info, parent)
    call inverse_permutate(perm, tmp_b, b)

  end subroutine

  subroutine hppllt_finalize()
    use starpu_wrapper_m, only: starpu_finalize
    use tasks_m, only: task_finalize => finalize
    use options_m
    
    deallocate(options)
    call task_finalize
    call starpu_finalize

  end subroutine
end module