module block_arrays_m
  use contiguous_sets_m
  use jagged_array_m
  use array_extractor_m
  use node_data_m
  implicit none
  private
  type, public :: block_arrays_c
    private
    double precision, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: ptr
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    class(extractor_c), pointer :: controller
    integer :: nb
  contains
    procedure :: set_val
    procedure :: set_zero
    procedure :: get_ptr
  end type

  public :: create_block_arrays

contains
  function create_block_arrays(nb, supernode_size, work_size, controller) result(this)
    type(block_arrays_c), pointer :: this
    integer, intent(in) :: nb
    integer, contiguous, target :: supernode_size(:), work_size(:)
    integer, pointer, contiguous :: array_size(:)
    class(extractor_c), pointer, intent(in) :: controller
    integer :: i, nc, nr

    allocate(this)
    allocate(array_size(size(supernode_size)))
    controller%node_data => create_node_data(supernode_size, work_size, nb)
    do i=1, size(supernode_size)
      array_size(i) = controller%estimate_size(i)
    enddo
    this%ptr => create_contiguous_sets(array_size)
    deallocate(array_size)
    allocate(this%val(this%ptr%get_num_elements()))
    this%controller => controller
    this%supernode_size => supernode_size
    this%work_size => work_size
    this%nb = nb

  end function

  !TODO valがすでにallocateされていたらdeallocate, supernodeはvalを割り当てないようにする
  subroutine set_val(this, val)
    class(block_arrays_c) :: this
    double precision, contiguous, target :: val(:)

    this%val => val

  end subroutine

  subroutine set_zero(this)
    class(block_arrays_c) :: this
    
    this%val = 0d0

  end subroutine

  function get_ptr(this, node, idx) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(block_arrays_c) :: this
    integer, intent(in) :: node, idx
    double precision, pointer, contiguous :: array(:)

    array => this%val(this%ptr%get_first(node):this%ptr%get_last(node))
    ptr => this%controller%get_ptr(array, node, idx)

  end function  
end module