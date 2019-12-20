module block_matrices_m
  use contiguous_sets_m
  use matrix_extractor_m
  use node_data_m
  implicit none
  private
  type, public :: block_matrices_c
    private
    double precision, pointer, contiguous :: val(:)
    type(contiguous_sets_c), pointer :: ptr
    class(extractor_c), pointer :: extractor
    integer :: nb
  contains
    procedure :: get_ptr
  end type

  public :: create_block_matrices

contains
  function create_block_matrices(node_data, extractor) result(this)
    type(block_matrices_c), pointer :: this
    type(node_data_c), pointer :: node_data
    integer, pointer, contiguous :: matrix_size(:)
    class(extractor_c), pointer, intent(in) :: extractor
    integer :: i, nc, nr

    allocate(this)
    allocate(matrix_size(node_data%num_node))
    this%extractor => extractor
    !TODO: コンストラクタ作成
    this%extractor%node_data => node_data
    do i=1, node_data%num_node
      matrix_size(i) = extractor%estimate_size(i)
    enddo
    this%ptr => create_contiguous_sets(matrix_size)
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