module fundamental_supernode_m
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  private

  public :: search_node_sets_in_supernode, create_supernodal_column_count
  public :: create_supernodal_ccs, create_supernodal_tree
  
contains
  function search_node_sets_in_supernode(isleaf, tree_child) result(node_sets)
    type(contiguous_sets_c) :: node_sets
    integer, pointer, contiguous, intent(in) :: isleaf(:)
    type(jagged_array_c), intent(in) :: tree_child
    integer, allocatable :: num_nodes(:)
    integer :: n, i, ptr, count, num_supernodes

    n = tree_child%get_num_arrays()
    
    count = 0
    do i=1, n
      if(isleaf(i) > 0 .or. tree_child%get_array_length(i) > 1)then
        count = count + 1
      endif
    enddo
    num_supernodes = count

    allocate(num_nodes(num_supernodes))
    num_nodes = 0
    ptr = 0
    do i=1, n
      if(isleaf(i) > 0 .or. tree_child%get_array_length(i) > 1)then
        ptr = ptr + 1
      endif
      num_nodes(ptr) = num_nodes(ptr) + 1
    enddo
    
    node_sets = create_contiguous_sets(num_nodes)

  end function

  function create_supernodal_column_count(node_sets, cc_node) result(cc_supernode)
    integer, pointer, contiguous :: cc_supernode(:)
    type(contiguous_sets_c), intent(in) :: node_sets
    integer, pointer, contiguous, intent(in) :: cc_node(:)
    integer :: i, n, last_node

    n = node_sets%get_num_sets()
    allocate(cc_supernode(n))
    do i=1, n
      last_node = node_sets%get_last(i)
      cc_supernode(i) = cc_node(last_node) - 1
    enddo

  end function

  function create_supernodal_tree(node_sets, tree_child) result(num_child_supernode)
    integer, pointer, contiguous :: num_child_supernode(:)
    type(contiguous_sets_c), intent(in) :: node_sets
    type(jagged_array_c), intent(in) :: tree_child
    integer :: n, i

    n = node_sets%get_num_sets()
    allocate(num_child_supernode(n))
    do i=1, n
      num_child_supernode(i) = tree_child%get_array_length(node_sets%get_first(i))
    enddo

  end function

  type(jagged_array_c) function create_supernodal_ccs(node_sets, ccs_node) result(ccs_supernode)
    type(contiguous_sets_c), intent(in) :: node_sets
    type(jagged_array_c) :: ccs_node
    integer, pointer, contiguous :: col(:), row(:), rows_node(:), rows_supernode(:)
    integer :: n, num_cols, num_vals, i

    n = node_sets%get_num_sets()
    allocate(col(n+1))
    col(1) = 1
    do i=1, n-1
      col(i+1) = col(i) + ccs_node%get_array_length(node_sets%get_first(i)) - node_sets%get_length(i)
    enddo
    col(n+1) = col(n)
    
    num_vals = col(n+1)-1
    allocate(row(num_vals))
    ccs_supernode = create_jagged_array(col, row)
    
    do i=1,n
      num_cols = node_sets%get_length(i)
      rows_node => ccs_node%get_array(node_sets%get_first(i))
      rows_supernode => ccs_supernode%get_array(i)
      rows_supernode = rows_node(num_cols+1:)
    enddo

  end function


end module