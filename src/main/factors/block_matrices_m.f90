module block_matrices_m
  use contiguous_sets_long_m
  use matrix_extractor_m
  use supernode_matrix_extractor_m
  use work_matrix_extractor_m
  use border_matrix_extractor_m
  use node_data_m
  use extractor_types_m
  implicit none
  private
  type, public :: block_matrices_c
    private
    double precision, pointer, contiguous :: val(:)
    type(contiguous_sets_long_c), pointer :: ptr
    class(extractor_c), pointer :: extractor
    integer :: nb
  contains
    procedure :: get_ptr
  end type

  public :: create_block_matrices

contains
  function create_block_matrices(node_data, extractor_type) result(this)
    type(block_matrices_c), pointer :: this
    type(node_data_c), pointer :: node_data
    integer, pointer, contiguous :: matrix_size(:)
    integer, intent(in) :: extractor_type
    integer :: i, nc, nr

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
    do i=1, node_data%num_node
      matrix_size(i) = this%extractor%estimate_size(i)
    enddo
    this%ptr => create_contiguous_sets_long(matrix_size)
    deallocate(matrix_size)
    allocate(this%val(this%ptr%get_num_elements()))
    
  end function

  function get_ptr(this, node, i, j) result(ptr)
    double precision, pointer, contiguous :: ptr(:)
    class(block_matrices_c) :: this
    integer, intent(in) :: node, i, j
    double precision, pointer, contiguous :: array(:)

    array => this%val(this%ptr%get_first(node):this%ptr%get_last(node))
    ptr => this%extractor%get_ptr(array, node, i, j)

  end function  
end module