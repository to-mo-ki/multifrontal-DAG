module supernode_m
  use jagged_array_m
  implicit none
  private

  public :: search_first_node_in_supernode
  public :: create_supernodal_ccs, create_supernodal_tree
  
contains
  function search_first_node_in_supernode(isleaf, tree_child) result(first_node)
    integer, pointer, contiguous :: first_node(:)
    integer, pointer, contiguous, intent(in) :: isleaf(:)
    type(jagged_array_c), intent(in) :: tree_child
    integer, allocatable :: tmp_first_node(:)
    integer :: n, i, ptr

    n = tree_child%get_num_arrays()
    allocate(tmp_first_node(n))
    
    ptr = 0
    do i=1, n
      if(isleaf(i) > 0 .or. tree_child%get_array_length(i) > 1)then
        ptr = ptr + 1
        tmp_first_node(ptr) = i
      endif
    enddo
    ptr = ptr + 1
    tmp_first_node(ptr) = n+1

    allocate(first_node(ptr))
    first_node = tmp_first_node(:ptr)

  end function

  function create_supernodal_tree(first_node, tree_child) result(num_child_supernode)
    integer, pointer, contiguous :: num_child_supernode(:)
    integer, pointer, contiguous, intent(in) :: first_node(:)
    type(jagged_array_c), intent(in) :: tree_child
    integer :: n, i

    n = size(first_node)-1
    allocate(num_child_supernode(n))
    do i=1, n
      num_child_supernode(i) = tree_child%get_array_length(first_node(i))
    enddo

  end function

  type(jagged_array_c) function create_supernodal_ccs(first_node, ccs_node) result(ccs_supernode)
    integer, pointer, contiguous, intent(in) :: first_node(:)
    type(jagged_array_c) :: ccs_node
    integer, pointer, contiguous :: col(:), row(:), rows_node(:), rows_supernode(:)
    integer :: n, num_cols, num_vals, i

    n = size(first_node)-1
    allocate(col(n+1))
    col(1) = 1
    do i=1, n-1
      num_cols = first_node(i+1) - first_node(i)
      col(i+1) = col(i) + ccs_node%get_array_length(first_node(i)) - num_cols
    enddo
    col(n+1) = col(n)
    
    num_vals = col(n+1)-1
    allocate(row(num_vals))
    ccs_supernode = create_jagged_array(col, row)
    
    do i=1,n
      num_cols = first_node(i+1) - first_node(i)
      rows_node => ccs_node%get_array(first_node(i))
      rows_supernode => ccs_supernode%get_array(i)
      rows_supernode = rows_node(num_cols+1:)
    enddo

  end function


end module