module tree_pruning_m
  use contiguous_sets_m
  use jagged_array_m
  use doubly_linked_list_m
  use iterator_m
  use heap_m
  implicit none
  private
  
  public :: tree_pruning

contains
  recursive function tree_pruning(cc, node_sets, tree_child, tol, p) result(subtree)
    integer, pointer, contiguous :: subtree(:)
    integer, pointer, contiguous, intent(in) :: cc(:)
    double precision, intent(in) :: tol
    integer, intent(in) :: p
    type(contiguous_sets_c), pointer, intent(in) :: node_sets
    type(jagged_array_c), pointer, intent(in) :: tree_child
    type(iterator_c), pointer :: iterator
    type(heap_c), pointer :: op_heap, bin_heap
    type(doubly_linked_list_c), pointer :: subtree_list
    integer, pointer, contiguous :: fcnt(:), nodewt(:), subtree_wt(:)
    double precision :: ratio
    integer :: ip, minbin, maxbin, maxop, n, ptr, err

    fcnt => count_op_count(cc, node_sets)
    nodewt => generate_tree_weights(fcnt, tree_child)
    n = size(cc)
    op_heap => create_max_heap(n)
    bin_heap => create_min_heap(n)
    subtree_list => create_doubly_linked_list(n)
    call subtree_list%add(n)
    allocate(subtree_wt(n))

    ratio = 0d0
    ip = p-1
    do while(ratio <= tol)
      ip = ip + 1
      call partition_tree(subtree_list, nodewt, tree_child, ip, err)
      if(err == 1)then
        print *, "Warning:no finding subtrees for tol=", tol
        subtree => tree_pruning(cc, node_sets, tree_child, 0d0, ip-1)
        return
      endif
      ptr = 0
      iterator => subtree_list%create_iterator()
      do while(iterator%has_next())
        ptr = ptr + 1
        subtree_wt(ptr) = nodewt(iterator%next())
      enddo
      call op_heap%build_heap(subtree_wt(1:ptr))
      call bin_heap%set_zero(ip)
      do while(.not. op_heap%is_empty())
        minbin = bin_heap%get_top_node()
        maxop = op_heap%get_top_node()
        call bin_heap%update_top_node(minbin+maxop)
        call op_heap%delete_top_node()
      enddo
      minbin = bin_heap%get_top_node()
      maxbin = bin_heap%max()
      ratio = dble(minbin)/dble(maxbin)
    enddo

    allocate(subtree(subtree_list%get_length()))
    iterator => subtree_list%create_iterator()
    ptr = 0
    do while(iterator%has_next())
      ptr = ptr + 1
      subtree(ptr) = iterator%next()
    enddo

  end function

  function count_op_count(cc, node_set) result(fcnt)
    integer, pointer, contiguous :: fcnt(:)
    integer, pointer, contiguous, intent(in) :: cc(:)
    type(contiguous_sets_c), pointer, intent(in) :: node_set
    integer :: n, i, k, l

    n = size(cc)
    allocate(fcnt(n))
    do i=1, n
      l = cc(i)
      k = node_set%get_length(i) + l
      fcnt(i) = k*(k+1)*(2*k+1)/6-l*(l+1)*(2*l+1)/6
    enddo

  end function

  function generate_tree_weights(fcnt, tree_child) result(nodewt)
    integer, pointer, contiguous :: nodewt(:)
    integer, pointer, contiguous, intent(in) :: fcnt(:)
    type(jagged_array_c), pointer, intent(in) :: tree_child
    integer :: n, i, j
    integer, pointer, contiguous :: childs(:)

    n = size(fcnt)
    allocate(nodewt(n))

    do i=1, n
      nodewt(i) = fcnt(i)
      if(tree_child%get_array_length(i) /= 0)then
        childs => tree_child%get_array(i)
        do j=1, size(childs)
          nodewt(i) = nodewt(i) + nodewt(childs(j))
        enddo
      endif
    enddo

  end function

  subroutine partition_tree(linked_list, nodewt, tree_child, ip, err)
    type(doubly_linked_list_c), pointer, intent(in) :: linked_list
    integer, pointer, contiguous, intent(in) :: nodewt(:)
    type(jagged_array_c), pointer, intent(in) :: tree_child
    integer, intent(in) :: ip
    integer :: node, i
    integer, pointer, contiguous :: childs(:)
    integer :: err

    err = 0
    do while(linked_list%get_length() < ip)
      node = find_node_with_max_op(linked_list, nodewt)
      if(node == 0)then
        err = 1
        return
      endif
      childs => tree_child%get_array(node)
      do i=1, size(childs)
        call linked_list%add(childs(i))
      enddo
      call linked_list%remove(node)
    enddo

  end subroutine

  integer function find_node_with_max_op(linked_list, nodewt) result(max_node)
    type(doubly_linked_list_c), pointer, intent(in) :: linked_list
    integer, pointer, contiguous, intent(in) :: nodewt(:)
    type(iterator_c), pointer :: iterator
    integer :: max_ops, node
    
    iterator => linked_list%create_iterator()
    max_ops = 0
    max_node = 0
    do while(iterator%has_next())
      node = iterator%next()
      if(nodewt(node) > max_ops)then
        max_ops = nodewt(node)
        max_node = node
      endif
    enddo

  end function

end module