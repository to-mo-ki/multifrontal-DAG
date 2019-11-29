module node_m
  implicit none
  private
  type, public :: node_c
    class(node_c), pointer :: next
    character(:), allocatable :: message, answer, check
  end type

  class(node_c), pointer :: first_node => null()
  class(node_c), pointer :: last_node => null()

  public :: add_node, reset_node, exist_node, disp_error

contains

  subroutine add_node(message, answer, check)
    character(*), intent(in) :: message
    character(*), intent(in) :: answer, check
    type(node_c), pointer :: new_node

    allocate(new_node)
    allocate(new_node%message, source=message)
    allocate(new_node%answer, source=answer)
    allocate(new_node%check, source=check)
    if(.not. associated(first_node))then
      first_node => new_node
    else
      last_node%next => new_node
    endif
    last_node => new_node

  end subroutine

  subroutine reset_node
    nullify(first_node)
    nullify(last_node)
  end subroutine

  logical function exist_node()
    exist_node = associated(first_node)
  end function

  subroutine disp_error()
    type(node_c), pointer :: node
    node => first_node
    do while(associated(node))
      write(*,*) node%message, "  ", trim(node%answer), ' is NOT EQUAL to ', trim(node%check)
      node => node%next
    enddo
  end subroutine

end module