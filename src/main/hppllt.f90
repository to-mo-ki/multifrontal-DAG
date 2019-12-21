module hppllt
  use hppllt_data_m
  implicit none
  private

  public :: hppllt_analyze, hppllt_factorize, hppllt_solve
  
contains

  subroutine hppllt_analyze(ccs_col, ccs_row, n, nb, max_zero)
    use analyze_phase_m
    use reordering_m
    use perm_m
    use create_local_index_m
    use create_supernodal_index_m
    use ordering_m
    integer, contiguous :: ccs_col(:), ccs_row(:)
    integer, intent(in) :: n, nb, max_zero
    type(contiguous_sets_c), pointer :: origin_set
    type(jagged_array_c), pointer :: l_structure
    type(jagged_array_c), pointer :: reordered_ccs
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    integer, pointer, contiguous :: reordering_perm(:), reordering_iperm(:), analyze_perm(:), analyze_iperm(:)
    integer, pointer, contiguous :: reordering_ccs_perm(:), analyze_ccs_perm(:)
    type(jagged_array_c), pointer :: local_index
    integer :: i
    
    origin_set => create_raw_contiguous_sets(ccs_col, n)
    origin_structure => create_jagged_array(origin_set, ccs_row)
    
    call Metis_ordering(origin_structure, reordering_perm, reordering_iperm)
    reordered_ccs => reordering_ccs(origin_structure, reordering_perm, reordering_iperm, reordering_ccs_perm)
    call analyze_phase(reordered_ccs, max_zero, l_structure, node_sets, analyze_perm, parent, tree_child)
    call perm_product(reordering_perm, analyze_perm, perm)
    !TODO : reorderingしないなら
    !perm => analyze_perm
    call set_iperm(analyze_perm, analyze_iperm)
    a_structure => repostordering_ccs(reordered_ccs, analyze_perm, analyze_iperm, analyze_ccs_perm)
    call perm_product(reordering_ccs_perm, analyze_ccs_perm, ccs_perm)
    
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
    block_local_index => create_block_local_index(node_data, local_index)

  end subroutine

  subroutine hppllt_factorize(a)
    use coefficient_setter_m
    use seq_factorize_m
    use perm_m
    use reordering_m
    use zero_setter_m
    double precision, pointer, contiguous :: a(:)
    double precision, pointer, contiguous :: ccs_val(:)

    allocate(ccs_val(size(ccs_perm)))
    call permutate(ccs_perm, a, ccs_val)
    ccs => create_ccs(supernodal_index, ccs_val)
    call set_zero(node_data, factors)
    call set_coefficient(node_data, ccs, node_sets, factors)
    call seq_factorize(node_data, factors, block_local_index, parent)

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
    call seq_forward(node_data, factors, rh, block_local_index, parent)
    call seq_backward(node_data, factors, rh, block_local_index, parent)
    call inverse_permutate(perm, tmp_b, b)

  end subroutine

end module