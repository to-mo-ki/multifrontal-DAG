module linked_list_m
  implicit none
  private
  type, public :: node_c
    type(node_c), pointer :: next
    character(:), allocatable :: message, answer, check
  end type
  type(node_c), pointer :: first_node => null()

  public :: add_node, reset_node, exist_node, get_first_node

contains

  subroutine add_node(message, answer, check)
    character(*), intent(in) :: message
    character(*), intent(in) :: answer, check
    type(node_c), pointer :: new_node, node

    allocate(new_node)
    allocate(new_node%message, source=message)
    allocate(new_node%answer, source=answer)
    allocate(new_node%check, source=check)
    if(.not. associated(first_node))then
      first_node => new_node
      return
    endif
    node => first_node
    do while(associated(node%next))
      node => node%next
    enddo
    node%next => new_node

  end subroutine

  subroutine reset_node
    nullify(first_node)
  end subroutine

  logical function exist_node()
    exist_node = associated(first_node)
  end function

  function get_first_node() result(node)
    type(node_c), pointer :: node
    node => first_node
  end function


end module