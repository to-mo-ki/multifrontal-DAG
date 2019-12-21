module block_arrays_m
  use contiguous_sets_m
  use jagged_array_m
  use array_extractor_m
  use supernode_array_extractor_m
  use work_array_extractor_m
  use border_array_extractor_m
  use node_data_m
  implicit none
  private
  integer, parameter, public :: SUPERNODE_EXTRACTOR = 1
  integer, parameter, public :: BORDER_EXTRACTOR = 2
  integer, parameter, public :: WORK_EXTRACTOR = 3
  type, public :: block_arrays_c
    private
    double precision, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: ptr
    class(extractor_c), pointer :: extractor
  contains
    procedure :: set_val
    procedure :: set_zero
    procedure :: get_ptr
  end type

  public :: create_block_arrays

contains
  function create_block_arrays(node_data, extractor_type) result(this)
    type(block_arrays_c), pointer :: this
    type(node_data_c), pointer :: node_data
    integer, intent(in) :: extractor_type
    integer, pointer, contiguous :: array_size(:)
    integer :: i, nc, nr

    allocate(this)
    allocate(array_size(node_data%num_node))
    select case(extractor_type)
      case(SUPERNODE_EXTRACTOR)
        allocate(supernode_extractor_c::this%extractor)
      case(BORDER_EXTRACTOR)
        allocate(border_extractor_c::this%extractor)
      case(WORK_EXTRACTOR)
        allocate(work_extractor_c::this%extractor)
      case default
        print *, "Unknown type"
        return
    end select
    this%extractor%node_data => node_data
    do i=1, node_data%num_node
      array_size(i) = this%extractor%estimate_size(i)
    enddo
    this%ptr => create_contiguous_sets(array_size)
    deallocate(array_size)
    allocate(this%val(this%ptr%get_num_elements()))

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
    ptr => this%extractor%get_ptr(array, node, idx)

  end function  
end module