module tree_m
  use jagged_array_m
  use stack_m
  implicit none
  private

  public :: create_tree_child, create_parent, tree_traverse_postordering, count_subtree_size

contains
  type(jagged_array_c) function create_tree_child(num_child, parent) result(tree_child)
    integer, pointer, contiguous, intent(in) :: num_child(:), parent(:)
    integer, pointer, contiguous :: childs(:), ptr(:)
    integer, allocatable :: child_pos(:)
    integer :: n, i

    n = size(num_child)
    allocate(childs(n), ptr(n+1), child_pos(n))

    ptr(1) = 1
    do i=1, n
      ptr(i+1) = ptr(i) + num_child(i)
    enddo

    child_pos = ptr(:n)
    do i=1, n
      if(parent(i) == 0) cycle
      childs(child_pos(parent(i))) = i
      child_pos(parent(i)) = child_pos(parent(i)) + 1
    enddo

    tree_child = create_jagged_array(ptr, childs)
  
  end function

  function create_parent(num_child) result(parent)
    ! TODO: no test(condition: postordering)
    integer, pointer, contiguous, intent(in) :: num_child(:)
    integer, pointer, contiguous :: parent(:)
    integer :: i, j, node, n
    type(stack_c) :: stack

    n = size(num_child)
    stack = create_stack(n)
    allocate(parent(n))
    parent = 0

    do i=1, n
      do j=1, num_child(i)
        node = stack%pop()
        parent(node) = i
      enddo
      call stack%push(i)
    enddo

  end function

  function tree_traverse_postordering(tree_child) result(perm)
    integer, pointer, contiguous :: perm(:), childs(:)
    type(jagged_array_c) :: tree_child
    integer :: ptr, i, node, n
    type(stack_c) :: stack

    n = tree_child%get_num_arrays()
    ptr = n
    stack = create_stack(n)
    allocate(perm(n))
    call stack%push(n)
    do while(.not. stack%is_empty())
      node = stack%pop()
      perm(ptr) = node
      ptr = ptr - 1
      childs => tree_child%get_array(node)
      do i=1, size(childs)
        call stack%push(childs(i))
      enddo
    enddo

  end function

  function count_subtree_size(tree_child) result(subtree_size)
    integer, pointer, contiguous :: subtree_size(:), childs(:)
    type(jagged_array_c) :: tree_child
    integer :: i, j, n
    
    n = tree_child%get_num_arrays()
    allocate(subtree_size(n))
    do i=1, n
      subtree_size(i) = 1
      childs => tree_child%get_array(i)
      do j=1, size(childs)
        subtree_size(i) = subtree_size(i) + subtree_size(childs(j))
      enddo
    enddo

  end function

end module