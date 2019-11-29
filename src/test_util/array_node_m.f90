module array_node_m
  use node_m, only: node_c
  use to_str_m
  implicit none
  private
  type, public :: array_node_c
    type(array_node_c), pointer :: next
    character(:), allocatable :: message
    type(node_c), pointer :: first_node=>null()
    type(node_c), pointer :: last_node=>null()
    logical :: size_error = .false.
    integer :: answer_size, check_size
  contains
    procedure :: add
  end type

  type(array_node_c), pointer :: first_node => null()
  type(array_node_c), pointer :: last_node => null()

  public :: add_size_error_node, add_node, reset_node, exist_node, add_array_err, disp_error

contains

  subroutine add_size_error_node(message, answer_size, check_size)
    character(*) :: message
    type(array_node_c), pointer :: new_node
    integer :: answer_size, check_size

    allocate(new_node)
    allocate(new_node%message, source=message)
    new_node%size_error = .true.
    new_node%answer_size = answer_size
    new_node%check_size = check_size
    if(.not. associated(first_node))then
      first_node => new_node
    else
      last_node%next => new_node
    endif
    last_node => new_node

  end subroutine

  subroutine add_node(message)
    character(*) :: message
    type(array_node_c), pointer :: new_node
    
    allocate(new_node)
    allocate(new_node%message, source=message)
    if(.not. associated(first_node))then
      first_node => new_node
    else
      last_node%next => new_node
    endif
    last_node => new_node

  end subroutine

  subroutine add_array_err(idx, answer, check)
    integer, intent(in) :: idx
    character(*), intent(in) :: answer, check
    call last_node%add(idx, answer, check)
  end subroutine

  subroutine reset_node
    nullify(first_node)
    nullify(last_node)
  end subroutine

  logical function exist_node()
    exist_node = associated(first_node)
  end function

  subroutine add(this, idx, answer, check)
    class(array_node_c) :: this
    integer, intent(in) :: idx
    character(*), intent(in) :: answer, check
    type(node_c), pointer :: new_node, node

    allocate(new_node)
    allocate(new_node%message, source=trim(to_str(idx))//"-th element ")
    allocate(new_node%answer, source=answer)
    allocate(new_node%check, source=check)
    if(.not. associated(this%first_node))then
      this%first_node => new_node
    else
      this%last_node%next => new_node
    endif
    this%last_node => new_node

  end subroutine

  subroutine disp_error()
    type(array_node_c), pointer :: array_node
    type(node_c), pointer :: node
    array_node => first_node
    do while(associated(array_node))
      if(array_node%size_error)then
        write(*,*) array_node%message, "  different of array size ", "answer:", trim(to_str(array_node%answer_size)), " check:", trim(to_str(array_node%check_size))
        array_node => array_node%next
        cycle
      endif
      write(*,*) array_node%message
      node => array_node%first_node
      do while(associated(node))
        write(*,*) node%message, "  ", trim(node%answer), ' is NOT EQUAL to ', trim(node%check)
        node => node%next
      enddo
      array_node => array_node%next
    enddo
    
  end subroutine

end module