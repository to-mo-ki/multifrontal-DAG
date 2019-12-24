module block_matrix_ptrs_m
  use contiguous_sets_m
  use starpu_matrix_extractor_m
  use work_starpu_matrix_extractor_m
  use border_starpu_matrix_extractor_m
  use supernode_starpu_matrix_extractor_m
  use node_data_m
  use iso_c_binding, only: c_ptr
  implicit none
  private
  integer, parameter, public :: SUPERNODE_EXTRACTOR = 1
  integer, parameter, public :: BORDER_EXTRACTOR = 2
  integer, parameter, public :: WORK_EXTRACTOR = 3
  type, public :: block_matrix_ptrs_c
    private
    type(c_ptr), pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: ptr
    integer, pointer, contiguous :: supernode_size(:), work_size(:)
    class(extractor_c), pointer :: extractor
    integer :: nb
  contains
    procedure :: get
  end type

  public :: create_block_matrix_ptrs

contains
  function create_block_matrix_ptrs(node_data, extractor_type) result(this)
    type(block_matrix_ptrs_c), pointer :: this
    type(node_data_c), pointer :: node_data
    integer, intent(in) :: extractor_type
    integer, pointer, contiguous :: matrix_size(:)
    integer :: node

    allocate(this)
    allocate(matrix_size(node_data%num_node))
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
    do node=1, node_data%num_node
      matrix_size(node) = this%extractor%estimate_size(node)
    enddo
    this%ptr => create_contiguous_sets(matrix_size)
    allocate(this%val(this%ptr%get_num_elements()))
    
  end function

  type(c_ptr) function get(this, node, i, j) result(ptr)
    class(block_matrix_ptrs_c) :: this
    integer, intent(in) :: node, i, j
    type(c_ptr), pointer, contiguous :: array(:)

    array => this%val(this%ptr%get_first(node):this%ptr%get_last(node))
    ptr = array(this%extractor%get_pos(node, i, j))

  end function  
end module