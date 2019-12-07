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
    integer, contiguous :: ccs_col(:), ccs_row(:)
    integer, intent(in) :: n, nb, max_zero
    type(contiguous_sets_c), pointer :: origin_set
    type(jagged_array_c), pointer :: l_structure
    type(jagged_array_c), pointer :: reordered_ccs
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    integer, pointer, contiguous :: reordering_perm(:), analyze_perm(:), iperm(:)
    integer :: i
    
    origin_set => create_raw_contiguous_sets(ccs_col, n)
    origin_structure => create_jagged_array(origin_set, ccs_row)
    
    ! TODO:reordering
    !reordered_ccs => reordering_ccs(origin_structure, reordering_perm, reordering_iperm)
    reordered_ccs => origin_structure

    call analyze_phase(reordered_ccs, max_zero, l_structure, node_sets, analyze_perm, parent)
    ! TODO:reordering
    !call perm_product(reordering_perm, analyze_perm, perm)
    perm => analyze_perm
    call set_iperm(perm, iperm)
    
    a_structure => reordering_ccs(origin_structure, perm, iperm)
    
    ! TODO:node_sets%get_lengthsを作成する？
    allocate(supernode_size(node_sets%get_num_sets()))
    do i=1, node_sets%get_num_sets()
      supernode_size(i) = node_sets%get_length(i)
    enddo
    allocate(work_size(node_sets%get_num_sets()))

    work_size = l_structure%get_array_lengths()
    node_data => create_node_data(supernode_size, work_size, nb)
    factors => create_factors(node_data, nb)
    rh => create_right_hand(node_data, nb)

  end subroutine

  subroutine hppllt_factorize(a)
    use coefficient_setter_m
    use seq_factorize_m
    use perm_m
    use reordering_m
    double precision, pointer, contiguous :: a(:)
    double precision, pointer, contiguous :: ccs_val(:)

    ccs_val => reordering_ccs_val(origin_structure%get_set(), a_structure%get_set(), a, perm)
    ccs => create_ccs(a_structure, ccs_val)
    
    call set_coefficient(node_data, ccs, node_sets, factors)
    call seq_factorize(node_data, factors, block_local_index, parent)

  end subroutine

  subroutine hppllt_solve(b)
    use seq_forward_m
    use seq_backward_m
    use right_hand_m
    double precision, contiguous :: b(:)

    call rh%set_val(b)
    call seq_forward(node_data, factors, rh, block_local_index, parent)
    call seq_backward(node_data, factors, rh, block_local_index, parent)

  end subroutine

end module