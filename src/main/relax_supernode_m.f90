module relax_supernode_m
  use jagged_array_m
  use doubly_linked_lists_m
  use contiguous_sets_m
  use iterator_m
  use stack_m
  implicit none
  private

  public :: compute_merge_lists
  
contains
  type(doubly_linked_lists_c) function compute_merge_lists(cc, tree_child, node_sets, max_zero) result(merge_lists)
    integer, pointer, contiguous, intent(in) :: cc(:)
    type(contiguous_sets_c) :: node_sets
    type(jagged_array_c), intent(in) :: tree_child
    integer, intent(in) :: max_zero
    integer :: n, i, j, k, min_zero, child, additional_zero, merge_node
    integer, pointer, contiguous :: childs(:)
    integer, allocatable :: num_cols(:), num_zeros(:)
    type(doubly_linked_lists_c) :: son_lists
    type(iterator_c) :: iterator
    type(stack_c) :: stack
    
    n = size(cc)
    son_lists = create_doubly_linked_lists(n)
    merge_lists = create_doubly_linked_lists(n)
    do i=1, n
      call merge_lists%add(i, i)
    enddo

    allocate(num_cols(n))
    do i=1, n
      num_cols(i) = node_sets%get_length(i)
    enddo
    allocate(num_zeros(n))
    stack = create_stack(n)
    do k=1, n
      if(tree_child%get_array_length(k) == 0) cycle
      childs => tree_child%get_array(k)
      do j=1, size(childs)
        call son_lists%add(childs(j), k)
      enddo
      do
        merge_node = choose_merge_node(k, max_zero, son_lists, cc, num_zeros, num_cols, additional_zero)
        if(merge_node /= 0)then
          call merge_lists%merge(merge_node, k)
          call son_lists%remove(merge_node, k)
          call stack%push(merge_node)
          num_zeros(k) = additional_zero
          num_cols(k) = num_cols(k) + num_cols(merge_node)
        else
          do while(.not. stack%is_empty())
            merge_node = stack%pop()
            call son_lists%merge(merge_node, k)
          enddo
          exit
        endif
      enddo
    enddo



  end function

  integer function choose_merge_node(parent, max_zero, son_lists, cc, num_zeros, num_cols, min_zero) result(merge_node)
    integer, intent(in) :: parent, max_zero, cc(:), num_zeros(:), num_cols(:)
    integer, intent(out) :: min_zero
    type(doubly_linked_lists_c), intent(in) :: son_lists
    integer :: child, additional_zero
    type(iterator_c) :: iterator

    min_zero = max_zero
    merge_node = 0
    iterator = son_lists%create_iterator(parent)
    do while(iterator%has_next())
      child = iterator%next()
      additional_zero = get_additional_zero(parent, child, cc, num_cols) + num_zeros(parent) + num_zeros(child)
      if(additional_zero <= min_zero .and. additional_zero <= max_zero)then
        merge_node = child
        min_zero = additional_zero
      endif
    enddo

  end function

  integer function get_additional_zero(parent, child, cc, ncol) result(additional_zero)
      integer, intent(in) :: parent, child, cc(:), ncol(:)
      additional_zero = (cc(parent) + ncol(parent) - cc(child)) * ncol(child)
    end function
end module