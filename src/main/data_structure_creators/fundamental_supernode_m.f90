module fundamental_supernode_m
  use jagged_array_m
  use contiguous_sets_m
  implicit none
  private

  public :: search_node_sets_in_supernode, create_supernodal_column_count
  public :: create_supernodal_tree, create_supernodal_ccs
  
contains
  function search_node_sets_in_supernode(isleaf, tree_child) result(node_sets)
    type(contiguous_sets_c), pointer :: node_sets
    integer, pointer, contiguous, intent(in) :: isleaf(:)
    type(jagged_array_c), pointer, intent(in) :: tree_child
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
    
    node_sets => create_contiguous_sets(num_nodes)

  end function

  function create_supernodal_column_count(node_sets, cc_node) result(cc_supernode)
    integer, pointer, contiguous :: cc_supernode(:)
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
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
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    type(jagged_array_c), pointer, intent(in) :: tree_child
    integer :: n, i

    n = node_sets%get_num_sets()
    allocate(num_child_supernode(n))
    do i=1, n
      num_child_supernode(i) = tree_child%get_array_length(node_sets%get_first(i))
    enddo

  end function


  function create_supernodal_ccs(node_sets, ccs_node) result(ccs_supernode)
    ! HACK: サブルーチン化
    type(jagged_array_c), pointer :: ccs_supernode
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    type(jagged_array_c), pointer, intent(in) :: ccs_node
    integer, pointer, contiguous :: rows_node(:), rows_supernode(:)
    integer :: n, row_num, i, j, k, num_supernode
    integer, allocatable :: full_array(:), num_row(:), row_ptr(:)

    n = ccs_node%get_num_arrays()
    num_supernode = node_sets%get_num_sets()
    allocate(num_row(num_supernode), full_array(n), row_ptr(num_supernode))
    num_row = 0
    full_array = 0
    do k=1, num_supernode
      do j=node_sets%get_first(k), node_sets%get_last(k)
        rows_node => ccs_node%get_array(j)
        do i=1, size(rows_node)
          row_num = rows_node(i)
          if(row_num <= node_sets%get_last(k) .or. full_array(row_num) == k)then
            cycle
          endif
          full_array(row_num) = k
          num_row(k) = num_row(k) + 1
        enddo
      enddo
    enddo
    ccs_supernode => create_jagged_array(num_row)
    
    full_array = 0
    row_ptr = 1
    do k=1, num_supernode
      rows_supernode => ccs_supernode%get_array(k)
      do j=node_sets%get_first(k), node_sets%get_last(k)
        rows_node => ccs_node%get_array(j)
        do i=1, size(rows_node)
          row_num = rows_node(i)
          if(row_num <= node_sets%get_last(k) .or. full_array(row_num) == k)then
            cycle
          endif
          full_array(row_num) = k
          rows_supernode(row_ptr(k)) = row_num
          row_ptr(k) = row_ptr(k) + 1
        enddo
      enddo
    enddo

  end function


end module